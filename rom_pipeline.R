#!/usr/bin/env Rscript
# =============================================================================
#  ROM export pipeline - reproducible reference implementation
# =============================================================================
#
#  PURPOSE
#  Rebuilds the analysis dataset from the ORIGINAL, UNMODIFIED questionnaire
#  exports. This script is the citable definition of how the raw exports become
#  the analysed data: given the same files in raw/ and the same config.json, it
#  reproduces the dataset exactly, on any machine, with no state carried over
#  between runs.
#
#  It is an INDEPENDENT implementation of the same specification as
#  merge_rom.py - written from the spec, not calling the Python code - so that
#  agreement between the two is evidence the specification is unambiguous.
#  verify_equivalence.R checks that agreement value by value.
#
#  METHOD (the part that belongs in a methods section)
#  1. Every export ever downloaded is retained in raw/ and all of them are read
#     on every run. Nothing is merged incrementally, so the output is a pure
#     function of the inputs.
#  2. The wide export is reshaped to one row per
#     (patient, questionnaire, occasion, field).
#     A row is emitted whenever the questionnaire instance carries a DATE, even
#     if the item is blank. A blank is therefore informative: it means the
#     questionnaire was administered and that item was left unanswered. No row
#     at all means the questionnaire was never administered.
#  3. Occasions are renumbered by date within (patient, questionnaire), because
#     the instance numbers in the export are positional and can shift between
#     downloads if a record is deleted upstream.
#  4. When the same measurement appears in several exports with different
#     values, the most recent export wins and every disagreement is logged to
#     value_changes.csv.
#  5. Exclusions are two-tiered. Confident rules remove a patient; weaker rules
#     only flag one. A patient with more than `never_exclude_above_occasions`
#     measurement occasions is never removed by an automatic rule - it is
#     downgraded to a flag - so that a placeholder string in a free-text field
#     cannot silently delete a real longitudinal case.
#  6. An item counts toward completeness only if it is in use: answered in at
#     least `active_item_min_response_rate` of the occasions on which its
#     questionnaire got any answer at all. Dead and conditional items would
#     otherwise mark every occasion incomplete.
#
#  USAGE
#      Rscript rom_pipeline.R              # writes output_R/
#      Rscript rom_pipeline.R --out DIR
#
#  REQUIREMENTS  R >= 4.0 and the jsonlite package. Nothing else.
# =============================================================================

suppressWarnings(suppressMessages(library(jsonlite)))

# The data contain Hebrew. Under a C/POSIX locale R cannot represent it and
# read.csv fails with "invalid input found on input connection", so ask for a
# UTF-8 locale up front and say so plainly if the machine has none.
if (!isTRUE(l10n_info()$`UTF-8`)) {
  for (loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "he_IL.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") break
  }
}
if (!isTRUE(l10n_info()$`UTF-8`))
  warning("No UTF-8 locale found; Hebrew text may not round-trip correctly.")

args     <- commandArgs(trailingOnly = TRUE)
BASE     <- dirname(normalizePath(sub("^--file=", "",
              grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
# The archive may live outside this folder - e.g. a Drive folder marked
# "available offline", so one archive serves every machine. Both pipelines read
# the same setting, or the equivalence check would compare different inputs.
RAW      <- local({
  d <- tryCatch(fromJSON(file.path(BASE, "config.json"),
                         simplifyVector = TRUE)$raw_dir, error = function(e) NULL)
  if (is.null(d) || !nzchar(trimws(d))) file.path(BASE, "raw") else path.expand(trimws(d))
})
CONFIG   <- file.path(BASE, "config.json")
OUT      <- if (length(args) >= 2 && args[1] == "--out") args[2] else file.path(BASE, "output_R")

META_COLS <- c("Patient uuid", "Clinic Name", "Gender", "Comorbidities",
               "Collaborators", "Age", "Case Manager", "Intake Summary",
               "Intake Date", "Notes", "Primary Diagnosis", "Treatment Start",
               "Medication")
FIELD_HE <- c("Notes" = "הערות", "Intake Summary" = "סיכום אינטייק",
              "Case Manager" = "מנהל מקרה", "Collaborators" = "משתתפים")

say <- function(...) cat(..., "\n", sep = "")

# ---------------------------------------------------------------- helpers

# Chronological order of exports: the timestamp in the filename, else mtime.
file_sort_key <- function(paths) {
  m <- regmatches(basename(paths),
                  regexpr("[0-9]{8}([_-]?[0-9]{4})?", basename(paths)))
  key <- rep(NA_real_, length(paths))
  for (i in seq_along(paths)) {
    nm <- basename(paths[i])
    mm <- regmatches(nm, regexec("([0-9]{8})[_-]?([0-9]{4})?", nm))[[1]]
    if (length(mm) >= 2 && !is.na(mm[2]) && nzchar(mm[2])) {
      hhmm <- if (length(mm) >= 3 && !is.na(mm[3]) && nzchar(mm[3])) mm[3] else "0000"
      d <- as.POSIXct(paste0(mm[2], hhmm), format = "%Y%m%d%H%M", tz = "UTC")
      if (!is.na(d)) key[i] <- as.numeric(d)
    }
    if (is.na(key[i])) key[i] <- as.numeric(file.info(paths[i])$mtime) + 1e12
  }
  order(key, basename(paths))
}

read_export <- function(path) {
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con))
  df <- read.csv(con, check.names = FALSE, colClasses = "character",
                 na.strings = c("", "NA"), stringsAsFactors = FALSE)
  names(df) <- sub("^﻿", "", names(df))   # strip the byte-order mark
  df
}

blank_to_na <- function(x) { x[!is.na(x) & trimws(x) == ""] <- NA; x }

# Three distinct states, and conflating them loses the measure the study needs:
#   answered      - the patient gave this item a value
#   blank         - the questionnaire WAS answered, this item was left empty
#   not_answered  - the questionnaire was sent and never answered at all; the
#                   source writes a literal token (default "N/A") in every field
# A generic CSV reader turns BOTH "N/A" and "" into missing, which merges
# item-level skipping into questionnaire-level non-response.
ST_ANSWERED <- "answered"; ST_BLANK <- "blank"; ST_NOT <- "not_answered"

classify <- function(x, nat) {
  v <- ifelse(is.na(x), "", trimws(x))
  ifelse(v %in% nat, ST_NOT, ifelse(v == "", ST_BLANK, ST_ANSWERED))
}

# Keys rebuilt from tapply names come back with "unknown" encoding, which the
# radix sort refuses. Re-mark them so ordering is byte-stable and locale-free.
as_utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }

# ---------------------------------------------------------------- reshape

wide_to_long <- function(df, source_name, nat) {
  cn <- names(df)
  m  <- regexec("^(.+?)[[:space:]]+instance[[:space:]]+([0-9]+)[[:space:]]+(.+)$",
                cn, perl = TRUE)
  parts <- regmatches(cn, m)
  ok <- lengths(parts) == 4L
  if (!any(ok)) return(NULL)

  q_of  <- trimws(vapply(parts[ok], `[`, "", 2))
  i_of  <- as.integer(vapply(parts[ok], `[`, "", 3))
  f_of  <- trimws(vapply(parts[ok], `[`, "", 4))
  col_of <- cn[ok]

  key <- paste(q_of, i_of, sep = "\r")
  is_date <- f_of == "Date"
  date_col <- setNames(col_of[is_date], key[is_date])

  uuid <- df[["Patient uuid"]]
  chunks <- vector("list", sum(!is_date))
  k <- 0L

  for (kk in unique(key)) {
    dc <- date_col[[kk]]
    if (is.null(dc)) next
    dates <- as.Date(blank_to_na(df[[dc]]), format = "%d/%m/%Y")
    keep <- which(!is.na(dates))
    if (!length(keep)) next
    idx <- which(key == kk & !is_date)
    for (j in idx) {
      k <- k + 1L
      raw <- df[[col_of[j]]][keep]
      st  <- classify(raw, nat)
      chunks[[k]] <- list(
        patient_uuid  = uuid[keep],
        questionnaire = rep(q_of[j], length(keep)),
        instance      = rep(i_of[j], length(keep)),
        date          = dates[keep],
        field         = rep(f_of[j], length(keep)),
        value         = ifelse(st == ST_ANSWERED, trimws(raw), NA_character_),
        status        = st)
    }
  }
  chunks <- chunks[seq_len(k)]
  if (!k) return(NULL)

  long <- data.frame(
    patient_uuid  = unlist(lapply(chunks, `[[`, "patient_uuid"), use.names = FALSE),
    questionnaire = unlist(lapply(chunks, `[[`, "questionnaire"), use.names = FALSE),
    instance      = unlist(lapply(chunks, `[[`, "instance"), use.names = FALSE),
    date          = as.Date(unlist(lapply(chunks, `[[`, "date"), use.names = FALSE),
                            origin = "1970-01-01"),
    field         = unlist(lapply(chunks, `[[`, "field"), use.names = FALSE),
    value         = unlist(lapply(chunks, `[[`, "value"), use.names = FALSE),
    status        = unlist(lapply(chunks, `[[`, "status"), use.names = FALSE),
    stringsAsFactors = FALSE)
  long$source_file <- source_name

  # Stable occasion number: rank a patient's instances of a questionnaire by
  # date, so the key survives renumbering between exports.
  u <- unique(long[, c("patient_uuid", "questionnaire", "instance", "date")])
  u <- u[order(u$patient_uuid, u$questionnaire, u$date, u$instance,
               method = "radix"), ]
  grp <- paste(u$patient_uuid, u$questionnaire, sep = "\r")
  u$occasion <- ave(seq_len(nrow(u)), grp, FUN = seq_along)
  key_long <- paste(long$patient_uuid, long$questionnaire, long$instance,
                    long$date, sep = "\r")
  key_u    <- paste(u$patient_uuid, u$questionnaire, u$instance, u$date, sep = "\r")
  long$occasion <- u$occasion[match(key_long, key_u)]
  long
}

# ---------------------------------------------------------------- cleaning

text_hits <- function(meta, cfg, patterns) {
  out <- rep("", nrow(meta))
  if (!length(patterns)) return(out)
  for (col in cfg$test_text_fields) {
    if (!col %in% names(meta)) next
    v <- trimws(ifelse(is.na(meta[[col]]), "", meta[[col]]))
    hit <- nzchar(v) & Reduce(`|`, lapply(patterns,
              function(p) grepl(p, v, ignore.case = TRUE, perl = TRUE)))
    lab <- if (col %in% names(FIELD_HE)) FIELD_HE[[col]] else col
    out[hit & out == ""] <- paste0("טקסט מציין מקום בשדה ", lab)
  }
  out
}

flag_test_patients <- function(meta, cfg, occ_count) {
  n_occ <- as.integer(ifelse(is.na(occ_count[meta$`Patient uuid`]), 0,
                             occ_count[meta$`Patient uuid`]))
  guard <- as.integer(cfg$never_exclude_above_occasions %||% 0)

  excl <- text_hits(meta, cfg, cfg$test_text_patterns_exclude)

  staff <- tolower(cfg$test_staff_names)
  if (length(staff)) {
    joined <- tolower(paste(ifelse(is.na(meta$`Case Manager`), "", meta$`Case Manager`),
                            "|",
                            ifelse(is.na(meta$Collaborators), "", meta$Collaborators)))
    hit <- Reduce(`|`, lapply(staff, function(s) grepl(s, joined, fixed = TRUE)))
    excl[hit & excl == ""] <- "חשבון בדיקה של הצוות"
  }
  man_ex <- cfg$manual_exclude_uuids %||% character(0)
  excl[meta$`Patient uuid` %in% man_ex & excl == ""] <- "הוחרג ידנית"

  rev <- text_hits(meta, cfg, cfg$test_text_patterns_review)
  age <- suppressWarnings(as.numeric(meta$Age))
  bad_age <- !is.na(age) & (age < cfg$min_plausible_age | age > cfg$max_plausible_age)
  rev[bad_age & rev == ""] <- "גיל לא סביר"

  # safety guard: a heuristic never deletes a patient carrying real data
  if (guard > 0) {
    rescued <- excl != "" & n_occ >= guard & !(meta$`Patient uuid` %in% man_ex)
    rev[rescued] <- paste0("נשמר על ידי שומר הבטיחות — ", excl[rescued],
                           ", אך יש לו ", n_occ[rescued], " מדידות")
    excl[rescued] <- ""
  }
  excl[meta$`Patient uuid` %in% (cfg$manual_keep_uuids %||% character(0))] <- ""

  meta$exclude_reason <- excl
  meta$review_flag    <- rev
  meta$n_occasions    <- n_occ
  meta
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

normalize_case <- function(meta, cfg) {
  for (col in (cfg$normalize_text_case_columns %||% character(0))) {
    if (!col %in% names(meta)) next
    v <- ifelse(is.na(meta[[col]]), "", meta[[col]])
    nz <- v[nzchar(v)]
    if (!length(nz)) next
    # Most frequent spelling wins; ties broken alphabetically so the result
    # never depends on row order or on the language doing the counting.
    tb <- table(nz)
    tb <- tb[order(-as.integer(tb), names(tb), method = "radix")]
    canon <- character(0)
    for (val in names(tb)) {
      k <- tolower(trimws(val))
      if (is.na(canon[k])) canon[k] <- val
    }
    idx <- tolower(trimws(v))
    out <- ifelse(nzchar(v) & !is.na(canon[idx]), canon[idx], v)
    meta[[col]] <- ifelse(nzchar(out), out, NA_character_)
  }
  meta
}

# ------------------------------------------------------- derived summaries

build_item_usage <- function(long, cfg) {
  it <- long[grepl("^item[[:space:]]+[0-9]+$", long$field), ]
  okey <- paste(it$patient_uuid, it$questionnaire, it$occasion, sep = "\r")
  any_ans <- tapply(it$answered, okey, max)
  it <- it[any_ans[okey] == 1, ]
  g <- paste(it$questionnaire, it$field, sep = "\r")
  n   <- tapply(it$answered, g, length)
  yes <- tapply(it$answered, g, sum)
  sp <- do.call(rbind, strsplit(as_utf8(names(n)), "\r", fixed = TRUE))
  u <- data.frame(questionnaire = as_utf8(sp[, 1]), field = as_utf8(sp[, 2]),
                  occasions_answered_questionnaire = as.integer(n),
                  item_answered = as.integer(yes), stringsAsFactors = FALSE)
  u$response_rate <- round(u$item_answered / u$occasions_answered_questionnaire, 4)
  u$active <- as.integer(u$response_rate >= cfg$active_item_min_response_rate)
  u[order(u$questionnaire, u$field, method = "radix"), ]
}

build_occasions <- function(long, usage) {
  active <- paste(usage$questionnaire[usage$active == 1],
                  usage$field[usage$active == 1], sep = "\r")
  it <- long[grepl("^item[[:space:]]+[0-9]+$", long$field), ]
  it$is_active <- paste(it$questionnaire, it$field, sep = "\r") %in% active
  it$is_not    <- as.integer(it$status == ST_NOT)
  it$is_blank  <- as.integer(it$status == ST_BLANK)
  k <- paste(it$patient_uuid, it$questionnaire, it$occasion, it$date, sep = "\r")

  n_items    <- tapply(it$answered, k, length)
  n_answered <- tapply(it$answered, k, sum)
  n_blank    <- tapply(it$is_blank, k, sum)
  n_not      <- tapply(it$is_not, k, sum)
  n_act_it   <- tapply(it$is_active, k, sum)
  n_act_ans  <- tapply(it$answered * it$is_active, k, sum)

  sp <- do.call(rbind, strsplit(as_utf8(names(n_items)), "\r", fixed = TRUE))
  o <- data.frame(patient_uuid = as_utf8(sp[, 1]), questionnaire = as_utf8(sp[, 2]),
                  occasion = as.integer(sp[, 3]), date = sp[, 4],
                  n_items = as.integer(n_items), n_answered = as.integer(n_answered),
                  n_blank = as.integer(n_blank), n_not_answered = as.integer(n_not),
                  n_active_items = as.integer(n_act_it),
                  n_active_answered = as.integer(n_act_ans),
                  stringsAsFactors = FALSE)
  o$n_missing <- o$n_active_items - o$n_active_answered
  o$completeness <- ifelse(o$n_not_answered == o$n_items, "not_answered",
                    ifelse(o$n_answered == 0, "empty",
                    ifelse(o$n_missing <= 0, "complete", "partial")))
  o$pct_answered <- round(o$n_active_answered /
                          ifelse(o$n_active_items == 0, NA, o$n_active_items), 3)

  tot <- long[long$field == "total score",
              c("patient_uuid", "questionnaire", "occasion", "value")]
  o$total_score <- suppressWarnings(as.numeric(
    tot$value[match(paste(o$patient_uuid, o$questionnaire, o$occasion, sep = "\r"),
                    paste(tot$patient_uuid, tot$questionnaire, tot$occasion, sep = "\r"))]))
  o[order(o$patient_uuid, o$questionnaire, o$date, method = "radix"), ]
}

build_sessions <- function(occ, cfg) {
  weekly   <- cfg$weekly_questionnaires %||% character(0)
  required <- cfg$required_for_complete_week %||% weekly
  if (!length(weekly)) return(NULL)
  w <- occ[occ$questionnaire %in% weekly, ]
  if (!nrow(w)) return(NULL)

  pd <- unique(w[, c("patient_uuid", "date")])
  pd <- pd[order(pd$patient_uuid, pd$date, method = "radix"), ]
  kk <- paste(pd$patient_uuid, pd$date, sep = "\r")
  wk <- paste(w$patient_uuid, w$date, sep = "\r")

  # A questionnaire can legitimately appear twice on one date. Ranking the
  # states and taking the best is deterministic, and answers the question the
  # session table is asking: was the battery completed that day?
  RANK <- c(not_answered = 0L, empty = 1L, partial = 2L, complete = 3L)
  UNRANK <- names(RANK)
  for (q in weekly) {
    sub <- w[w$questionnaire == q, ]
    r <- tapply(unname(RANK[sub$completeness]),
                paste(sub$patient_uuid, sub$date, sep = "\r"), max)
    v <- r[kk]
    pd[[q]] <- ifelse(is.na(v), "missing", UNRANK[v + 1L])
  }
  M <- as.matrix(pd[, weekly, drop = FALSE])
  pd$n_present  <- rowSums(M != "missing")
  pd$n_complete <- rowSums(M == "complete")
  req <- intersect(required, weekly)
  R <- as.matrix(pd[, req, drop = FALSE])
  pd$week_complete <- as.integer(rowSums(R == "complete") == length(req))
  pd$week_present  <- as.integer(rowSums(R != "missing") == length(req))
  pd$session_index <- ave(seq_len(nrow(pd)), pd$patient_uuid, FUN = seq_along)
  pd$iso_week <- strftime(as.Date(pd$date), "%G-W%V")
  pd
}

build_coverage <- function(meta, occ, sessions) {
  b <- meta[, c("Patient uuid", "Gender", "Age", "Primary Diagnosis",
                "Case Manager", "Collaborators", "Intake Date", "Treatment Start")]
  names(b)[1] <- "patient_uuid"
  first_collab <- trimws(vapply(strsplit(ifelse(is.na(b$Collaborators), "",
                                                b$Collaborators), ","),
                                function(x) if (length(x)) x[1] else "", ""))
  th <- trimws(ifelse(is.na(b$`Case Manager`), "", b$`Case Manager`))
  th[th == ""] <- first_collab[th == ""]
  th[th == ""] <- "ללא שיוך"
  b$therapist <- th

  agg <- function(k, v, f) tapply(v, k, f)
  pu <- occ$patient_uuid
  b$n_occasions          <- as.integer(agg(pu, occ$occasion, length)[b$patient_uuid])
  b$n_questionnaires     <- as.integer(agg(pu, occ$questionnaire,
                                           function(x) length(unique(x)))[b$patient_uuid])
  b$n_complete_occasions <- as.integer(agg(pu, occ$completeness,
                                           function(x) sum(x == "complete"))[b$patient_uuid])
  b$n_partial_occasions  <- as.integer(agg(pu, occ$completeness,
                                           function(x) sum(x == "partial"))[b$patient_uuid])
  b$n_empty_occasions    <- as.integer(agg(pu, occ$completeness,
                                           function(x) sum(x == "empty"))[b$patient_uuid])
  b$n_not_answered_occasions <- as.integer(agg(pu, occ$completeness,
                                           function(x) sum(x == "not_answered"))[b$patient_uuid])
  b$n_responded_occasions    <- as.integer(agg(pu, occ$completeness,
                                           function(x) sum(x %in% c("complete","partial")))[b$patient_uuid])
  b$first_date <- agg(pu, occ$date, min)[b$patient_uuid]
  b$last_date  <- agg(pu, occ$date, max)[b$patient_uuid]

  if (!is.null(sessions) && nrow(sessions)) {
    sp <- sessions$patient_uuid
    b$n_sessions      <- as.integer(agg(sp, sessions$date,
                                        function(x) length(unique(x)))[b$patient_uuid])
    b$n_weeks_present <- as.integer(agg(sp, sessions$week_present, sum)[b$patient_uuid])
    b$n_weeks_complete<- as.integer(agg(sp, sessions$week_complete, sum)[b$patient_uuid])
    b$first_session   <- agg(sp, sessions$date, min)[b$patient_uuid]
    b$last_session    <- agg(sp, sessions$date, max)[b$patient_uuid]
  }
  for (q in sort(unique(occ$questionnaire))) {
    sub <- occ[occ$questionnaire == q, ]
    cnt <- table(sub$patient_uuid)
    b[[paste0("n::", q)]] <- as.integer(ifelse(is.na(cnt[b$patient_uuid]), 0,
                                               cnt[b$patient_uuid]))
  }
  num <- vapply(b, is.numeric, TRUE)
  b[num] <- lapply(b[num], function(x) ifelse(is.na(x), 0, x))
  b
}

# ---------------------------------------------------------------- main

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
cfg <- fromJSON(CONFIG, simplifyVector = TRUE)
NAT <- cfg$not_answered_tokens %||% "N/A"

files <- list.files(RAW, pattern = "\\.csv$", full.names = TRUE)
if (!length(files)) stop("No export files in raw/")
files <- files[file_sort_key(files)]
say("Rebuilding from ", length(files), " export(s): ",
    paste(basename(files), collapse = ", "))

longs <- list(); metas <- list()
for (i in seq_along(files)) {
  df <- read_export(files[i])
  lg <- wide_to_long(df, basename(files[i]), NAT)
  md <- df[, intersect(META_COLS, names(df)), drop = FALSE]
  md[] <- lapply(md, function(x) { x[trimws(ifelse(is.na(x), "", x)) %in% c("", NAT)] <- NA; x })
  md$source_file <- basename(files[i])
  lg$.ord <- i; md$.ord <- i
  longs[[i]] <- lg; metas[[i]] <- md
  say("  ", basename(files[i]), ": ", nrow(df), " patients, ", ncol(df),
      " cols -> ", format(nrow(lg), big.mark = ","), " rows")
}
long <- do.call(rbind, longs)
meta_all <- do.call(rbind, metas)

# ---- history: same measurement, different value between exports
key <- paste(long$patient_uuid, long$questionnaire, long$occasion,
             long$date, long$field, sep = "\r")
nun <- tapply(paste0(ifelse(is.na(long$value), "\001NA", long$value), "\037", long$status),
              key, function(x) length(unique(x)))
changed <- names(nun)[nun > 1]
ch <- long[key %in% changed, ]
CH_COLS <- c("patient_uuid", "questionnaire", "occasion", "date", "field",
             "source_file", "value", "status")
ch <- if (nrow(ch) > 0) {
  ch[order(ch$patient_uuid, ch$questionnaire, ch$occasion, ch$date,
           ch$field, ch$.ord, method = "radix"), CH_COLS]
} else {
  setNames(data.frame(matrix(character(0), 0, length(CH_COLS)),
                      stringsAsFactors = FALSE), CH_COLS)
}

# ---- newest export wins
o <- order(long$.ord, method = "radix")
long <- long[o, ]; key <- key[o]
last_i  <- tapply(seq_len(nrow(long)), key, function(ix) ix[length(ix)])
first_i <- tapply(seq_len(nrow(long)), key, function(ix) ix[1])
nexp    <- tapply(long$source_file, key, function(x) length(unique(x)))
agg <- data.frame(
  patient_uuid  = long$patient_uuid[last_i],
  questionnaire = long$questionnaire[last_i],
  occasion      = long$occasion[last_i],
  date          = long$date[last_i],
  field         = long$field[last_i],
  value         = long$value[last_i],
  status        = long$status[last_i],
  instance      = long$instance[last_i],
  first_seen_file = long$source_file[first_i],
  last_seen_file  = long$source_file[last_i],
  n_exports       = as.integer(nexp),
  stringsAsFactors = FALSE)

excl_q <- cfg$excluded_questionnaires %||% character(0)
n_dropped_q <- sum(agg$questionnaire %in% excl_q)
agg <- agg[!agg$questionnaire %in% excl_q, ]
agg$answered <- as.integer(agg$status == ST_ANSWERED)

occ_count <- table(unique(agg[, c("patient_uuid", "questionnaire",
                                  "occasion")])$patient_uuid)

meta_all <- meta_all[order(meta_all$.ord, method = "radix"), ]
li <- tapply(seq_len(nrow(meta_all)), meta_all$`Patient uuid`,
             function(ix) ix[length(ix)])
meta <- meta_all[sort(unlist(li)), setdiff(names(meta_all), ".ord")]
meta <- meta[order(meta$`Patient uuid`, method = "radix"), ]
meta <- normalize_case(meta, cfg)
meta <- flag_test_patients(meta, cfg, occ_count)

if (isTRUE(cfg$drop_patients_with_no_questionnaires)) {
  nodata <- !(meta$`Patient uuid` %in% unique(agg$patient_uuid)) &
            meta$exclude_reason == ""
  meta$exclude_reason[nodata] <- "אין נתוני שאלונים"
}

quarantine <- meta[meta$exclude_reason != "", ]
clean_meta <- meta[meta$exclude_reason == "",
                   setdiff(names(meta), "exclude_reason")]
clean <- agg[agg$patient_uuid %in% clean_meta$`Patient uuid`, ]
clean$date <- format(clean$date, "%Y-%m-%d")
clean <- clean[order(clean$patient_uuid, clean$questionnaire, clean$date,
                     clean$field, method = "radix"),
               c("patient_uuid", "questionnaire", "occasion", "date", "field",
                 "value", "status", "answered", "instance", "first_seen_file",
                 "last_seen_file", "n_exports")]

usage    <- build_item_usage(clean, cfg)
occ      <- build_occasions(clean, usage)
sessions <- build_sessions(occ, cfg)
coverage <- build_coverage(clean_meta, occ, sessions)
flagged  <- clean_meta[clean_meta$review_flag != "", ]

wr <- function(x, name) write.csv(x, file.path(OUT, name), row.names = FALSE,
                                  na = "", fileEncoding = "UTF-8")
wr(clean, "master_long.csv");        wr(clean_meta, "master_patients.csv")
wr(occ, "occasions.csv");            wr(coverage, "coverage_by_patient.csv")
wr(quarantine, "quarantine.csv");    wr(ch, "value_changes.csv")
wr(usage, "item_usage.csv");         wr(flagged, "review_needed.csv")
if (!is.null(sessions)) wr(sessions, "sessions.csv")

# ---- provenance: what exactly produced this dataset
prov <- list(
  run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  r_version = R.version.string,
  platform = R.version$platform,
  jsonlite = as.character(packageVersion("jsonlite")),
  config = fromJSON(CONFIG, simplifyVector = TRUE),
  inputs = lapply(files, function(f) list(
    file = basename(f), bytes = as.numeric(file.info(f)$size),
    md5 = unname(tools::md5sum(f)))),
  counts = list(
    patients_total = nrow(meta), patients_kept = nrow(clean_meta),
    patients_quarantined = nrow(quarantine), patients_flagged = nrow(flagged),
    rows_kept = nrow(clean), rows_with_value = sum(clean$answered),
    rows_blank_item_on_answered_form = sum(clean$status == ST_BLANK),
    rows_form_never_answered = sum(clean$status == ST_NOT),
    occasions_kept = nrow(occ),
    occasions_complete = sum(occ$completeness == "complete"),
    occasions_partial  = sum(occ$completeness == "partial"),
    occasions_empty    = sum(occ$completeness == "empty"),
    occasions_not_answered = sum(occ$completeness == "not_answered"),
    questionnaires_kept = length(unique(clean$questionnaire)),
    rows_dropped_excluded_questionnaire = n_dropped_q,
    values_changed_between_exports = length(changed),
    date_min = min(clean$date), date_max = max(clean$date)))
write(toJSON(prov, auto_unbox = TRUE, pretty = TRUE),
      file.path(OUT, "provenance.json"))

say("\n=== summary ===")
for (nm in names(prov$counts)) say("  ", nm, ": ", prov$counts[[nm]])
say("\nQuarantined by reason:")
if (nrow(quarantine)) {
  tb <- sort(table(quarantine$exclude_reason), decreasing = TRUE)
  for (nm in names(tb)) say("  ", nm, ": ", tb[[nm]])
} else say("  (none)")
say("\nOutputs in ", OUT)
