# =============================================================================
# Saving a result as CSV files and JSON
#
# CSV: complete, no row limit.
#   checks.csv          one row per check
#   issues_grouped.csv  one row per identical problem (column 'problem' = its number)
#   issues.csv          one row per issue and record: problem number, record ID,
#                       file, line, ... Written in chunks, so even 40 million rows
#                       never sit in memory at once. When there are more rows than
#                       Excel can open (1,048,575), it is written compressed as
#                       issues.csv.gz (about 15x smaller; open with R, Python or 7-Zip).
#   run.json            dataset, source, checksum, rules, date
#
# Written with a byte order mark (BOM) so Excel shows accents correctly. Excel
# with Belgian or German settings expects ';' between columns: use
# qc_export_csv(result, folder, sep = ";") for that.
#
# JSON: the whole result in one file, for pipelines (EurOBIS harvest, IT).
# =============================================================================

#' Save a QC result as CSV files
#'
#' Writes `checks.csv`, `issues_grouped.csv`, `issues.csv` (or `issues.csv.gz`
#' above Excel's row limit) and `run.json`.
#'
#' @param result A result of [qc_check()].
#' @param folder Folder to write to (created when needed).
#' @param sep Column separator: "," (default) or ";" for Excel with Belgian/German settings.
#' @return The folder path, invisibly.
#' @export
qc_export_csv <- function(result, folder, sep = ",") {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  write <- function(data, name) fwrite(data, file.path(folder, name), sep = sep, bom = TRUE, na = "")
  write(result$checks, "checks.csv")
  write(result$issues_grouped, "issues_grouped.csv")
  write_issue_detail(result, folder, sep)
  writeLines(jsonlite::toJSON(c(result$run, list(tables = result$tables)), auto_unbox = TRUE, pretty = TRUE, na = "null"),
             file.path(folder, "run.json"), useBytes = TRUE)
  invisible(folder)
}

# issues.csv (or .csv.gz), written in chunks of rows.
write_issue_detail <- function(result, folder, sep, chunk_size = 2e6, compress_above = excel_max_rows) {
  total <- sum(severity_counts(result))
  compress <- total > compress_above
  path <- file.path(folder, if (compress) "issues.csv.gz" else "issues.csv")
  unlink(file.path(folder, c("issues.csv", "issues.csv.gz")))

  rows <- result$issue_rows
  chunks <- if (nrow(rows)) split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / chunk_size)) else list(integer())
  for (i in seq_along(chunks)) {
    detail <- issues_as_table(result$dataset, result$issues_grouped, rows[chunks[[i]]], include_rowless = (i == 1))
    detail[, table_row := NULL]
    fwrite(detail, path, sep = sep, na = "", append = (i > 1), bom = (i == 1 && !compress),
           compress = if (compress) "gzip" else "none")
  }
  invisible(path)
}

#' Save a QC result as one JSON file
#'
#' @param result A result of [qc_check()].
#' @param file Path of the .json file to write.
#' @param include_detail Include one row per issue and record (`issues`).
#'   Default: only when there are no more than 1,048,575 issues; above that the
#'   file would be very large, and `issues_grouped` with [qc_export_csv()] is the better choice.
#' @return The file path, invisibly.
#' @export
qc_export_json <- function(result, file, include_detail = sum(severity_counts(result)) <= excel_max_rows) {
  content <- list(run = result$run, tables = result$tables, checks = result$checks,
                  issues_grouped = result$issues_grouped)
  if (include_detail) content$issues <- qc_issues(result)[, table_row := NULL]
  jsonlite::write_json(content, file, auto_unbox = TRUE, pretty = FALSE, na = "null", dataframe = "rows")
  invisible(file)
}
