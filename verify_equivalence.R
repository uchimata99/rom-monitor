#!/usr/bin/env Rscript
# =============================================================================
#  Equivalence check: R pipeline  vs.  Python pipeline
# =============================================================================
#
#  Two independent implementations of the same specification should produce the
#  same dataset. This script proves it, or shows exactly where they disagree.
#
#  It compares every output table cell by cell. Comparison is type-aware: a
#  column that parses as numeric in both is compared numerically with a
#  tolerance, because the two languages format numbers differently ("9" vs
#  "9.0") without meaning anything different. Everything else is compared as
#  trimmed text. Row order is not assumed - tables are re-sorted on their key
#  columns first - so the check is about content, not about how it was written.
#
#  Exit status is 0 only if every table matches, which makes it usable as a
#  regression gate before an analysis is run or a result is published.
#
#      Rscript verify_equivalence.R [dirA] [dirB]
#          defaults: output (Python)  and  output_R (R)
# =============================================================================

if (!isTRUE(l10n_info()$`UTF-8`))
  for (loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8"))
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") break

args <- commandArgs(trailingOnly = TRUE)
BASE <- dirname(normalizePath(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
A <- if (length(args) >= 1) args[1] else file.path(BASE, "output")     # python
B <- if (length(args) >= 2) args[2] else file.path(BASE, "output_R")   # R

TOL <- 1e-9
KEYS <- list(
  master_long.csv          = c("patient_uuid", "questionnaire", "occasion", "date", "field"),
  occasions.csv            = c("patient_uuid", "questionnaire", "occasion"),
  master_patients.csv      = c("Patient uuid"),
  coverage_by_patient.csv  = c("patient_uuid"),
  quarantine.csv           = c("Patient uuid"),
  review_needed.csv        = c("Patient uuid"),
  item_usage.csv           = c("questionnaire", "field"),
  sessions.csv             = c("patient_uuid", "date"),
  value_changes.csv        = c("patient_uuid", "questionnaire", "occasion", "date",
                               "field", "source_file"))

say <- function(...) cat(..., "\n", sep = "")
rd <- function(p) {
  con <- file(p, open = "r", encoding = "UTF-8"); on.exit(close(con))
  d <- read.csv(con, check.names = FALSE, colClasses = "character",
                na.strings = character(0), stringsAsFactors = FALSE)
  d[] <- lapply(d, function(x) { Encoding(x) <- "UTF-8"; ifelse(is.na(x), "", x) })
  d
}
is_num <- function(x) {
  nz <- x[nzchar(x)]
  length(nz) > 0 && !any(is.na(suppressWarnings(as.numeric(nz))))
}

files <- sort(union(basename(list.files(A, "\\.csv$")),
                    basename(list.files(B, "\\.csv$"))))
files <- setdiff(files, "run_log.csv")   # an append-only journal, not a dataset

say(strrep("=", 74))
say("  Equivalence check")
say("  A (python): ", A)
say("  B (R):      ", B)
say(strrep("=", 74), "\n")

overall_ok <- TRUE
summary_rows <- list()

for (f in files) {
  pa <- file.path(A, f); pb <- file.path(B, f)
  if (!file.exists(pa) || !file.exists(pb)) {
    say(sprintf("%-26s MISSING on %s", f, if (file.exists(pa)) "B" else "A"))
    overall_ok <- FALSE
    summary_rows[[f]] <- c(f, "MISSING", "", "")
    next
  }
  da <- rd(pa); db <- rd(pb)

  common <- intersect(names(da), names(db))
  only_a <- setdiff(names(da), names(db)); only_b <- setdiff(names(db), names(da))

  k <- KEYS[[f]]
  k <- if (is.null(k)) common else intersect(k, common)
  if (length(k)) {
    oa <- do.call(order, c(lapply(k, function(c) da[[c]]), list(method = "radix")))
    ob <- do.call(order, c(lapply(k, function(c) db[[c]]), list(method = "radix")))
    da <- da[oa, , drop = FALSE]; db <- db[ob, , drop = FALSE]
  }

  ok <- TRUE; notes <- character(0)
  if (nrow(da) != nrow(db)) {
    ok <- FALSE
    notes <- c(notes, sprintf("row count %d vs %d", nrow(da), nrow(db)))
  }
  if (length(only_a) || length(only_b)) {
    notes <- c(notes, sprintf("cols only in A: {%s}; only in B: {%s}",
                              paste(only_a, collapse = ","),
                              paste(only_b, collapse = ",")))
  }

  bad_cells <- 0; bad_cols <- character(0)
  if (nrow(da) == nrow(db) && nrow(da) > 0) {
    for (cn in common) {
      xa <- da[[cn]]; xb <- db[[cn]]
      if (is_num(xa) && is_num(xb)) {
        na_ <- suppressWarnings(as.numeric(ifelse(nzchar(xa), xa, NA)))
        nb_ <- suppressWarnings(as.numeric(ifelse(nzchar(xb), xb, NA)))
        diff <- !((is.na(na_) & is.na(nb_)) |
                  (!is.na(na_) & !is.na(nb_) & abs(na_ - nb_) <= TOL))
      } else {
        diff <- trimws(xa) != trimws(xb)
      }
      n <- sum(diff, na.rm = TRUE)
      if (n) {
        bad_cells <- bad_cells + n; bad_cols <- c(bad_cols, sprintf("%s(%d)", cn, n))
        if (length(bad_cols) <= 3) {
          i <- which(diff)[1]
          notes <- c(notes, sprintf("  e.g. %s row %d: A=%s | B=%s",
                                    cn, i, sQuote(xa[i]), sQuote(xb[i])))
        }
      }
    }
    if (bad_cells) ok <- FALSE
  }

  status <- if (ok) "MATCH" else "DIFFER"
  say(sprintf("%-26s %-7s %7s rows x %2d cols%s", f, status,
              format(nrow(da), big.mark = ","), length(common),
              if (bad_cells) sprintf("   %d differing cells", bad_cells) else ""))
  if (length(bad_cols)) say("    columns: ", paste(bad_cols, collapse = ", "))
  for (n in notes) say("    ", n)
  if (!ok) overall_ok <- FALSE
  summary_rows[[f]] <- c(f, status, nrow(da), bad_cells)
}

say("\n", strrep("=", 74))
if (overall_ok) {
  say("  RESULT: identical. The two implementations agree on every value.")
} else {
  say("  RESULT: differences found - see above.")
}
say(strrep("=", 74))

sess <- file.path(B, "verification.txt")
writeLines(c(
  paste("verified at:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  paste("result:", if (overall_ok) "IDENTICAL" else "DIFFERENCES FOUND"),
  paste("A (python):", A), paste("B (R):", B),
  "", "tables:",
  vapply(summary_rows, function(r) sprintf("  %-26s %-8s rows=%s diff_cells=%s",
                                           r[1], r[2], r[3], r[4]), ""),
  "", capture.output(sessionInfo())), sess)
say("\nWrote ", sess)
quit(status = if (overall_ok) 0 else 1)
