# =============================================================================
# The result of qc_check()
#
#   result$run             one list: dataset, source, checksum, rules, date, duration
#   result$tables          one row per table: file, records, columns, header row
#   result$checks          one row per check: status, issues, records affected, reason
#   result$issues_grouped  one row per identical problem: check, field, value,
#                          message, number of records, first line, example records
#   result$issue_rows      compact: problem number + row number per affected record
#   result$dataset         the data that was checked (needed to build the detail)
#
#   qc_issues(result, ...) the full detail: one row per issue and record, with
#                          record ID, file and line. Built only when asked for.
#
# These are the reporting layers of docs/design.md, section 4.7.
# Problem numbers follow importance: errors first, then the order of the checks.
# =============================================================================

build_result <- function(dataset, rules, plan, outcome, started) {
  problems <- outcome$problems
  rows <- outcome$rows

  records <- rows[, .N, by = problem]
  problems[, records := records$N[match(problem, records$problem)]]
  problems[is.na(records), records := 0L]
  problems <- add_examples(problems, rows, dataset)

  # Renumber the problems by importance; sort the rows the same way.
  new_order <- problems[order(-severity_rank[severity], match(check_id, plan$check_id), table, first_line), problem]
  problems[, problem := match(problem, new_order)]
  setorder(problems, problem)
  rows[, problem := match(problem, new_order)]
  setorder(rows, problem, row)

  checks <- plan[, intersect(c("check_id", "group", "title", "severity", "added_as_dependency",
                               "what", "why", "other_standards", "fields"), names(plan)), with = FALSE]
  checks <- merge(checks, outcome$checks, by = "check_id", sort = FALSE)
  checks <- merge(checks, summarise_per_check(problems, rows, dataset), by = "check_id", all.x = TRUE, sort = FALSE)
  checks[is.na(issues), `:=`(issues = 0L, records_affected = 0L, percent_affected = 0)]
  checks <- checks[order(match(status, status_levels), -severity_rank[severity], match(check_id, plan$check_id))]
  setcolorder(checks, c("check_id", "group", "title", "severity", "status", "issues", "records_affected",
                        "percent_affected", "reason", "duration_s", "added_as_dependency"))

  structure(list(
    run = list(
      tool            = "QC Tool (Beta)",
      tool_version    = as.character(utils::packageVersion("qctool")),
      dataset_title   = dataset$info$title,
      ipt_url         = dataset$info$ipt_url,
      source          = dataset$source$path,
      source_type     = dataset$source$type,
      source_name     = dataset$source$name,
      archive_url     = if (is.null(dataset$source$archive_url)) NA_character_ else dataset$source$archive_url,
      checksum_md5    = dataset$source$checksum,
      reading_notes   = dataset$notes,
      core            = dataset$core,
      rules_file      = rules$path,
      online          = !isFALSE(dataset$online),
      checked_at      = format(started, "%Y-%m-%d %H:%M:%S"),
      duration_s      = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    ),
    tables = dataset$files[, .(table, file, records, columns, core = is_core,
                               header_row = ifelse(header_lines > 0, "yes", "no"))],
    checks = checks,
    lookups = outcome$lookups,   # what was looked up online or taken from the cache
    issues_grouped = problems,
    issue_rows = rows,
    dataset = dataset
  ), class = "qc_result")
}

# First line and up to five example records per problem.
add_examples <- function(problems, rows, dataset) {
  first_rows <- rows[rowid(problem) <= 5L]
  pointers <- issue_pointers(dataset, problems, first_rows, include_rowless = FALSE)
  if (!nrow(pointers)) {
    return(problems[, `:=`(first_line = NA_integer_, example_records = NA_character_)])
  }
  pointers[, label := ifelse(is.na(measurement_type), record_id, paste0(record_id, " (", measurement_type, ")"))]
  examples <- pointers[, .(first_line = min(line), example_records = paste(unique(label[!is.na(label)]), collapse = "; ")),
                       by = problem]
  problems[, first_line := examples$first_line[match(problem, examples$problem)]]
  problems[, example_records := examples$example_records[match(problem, examples$problem)]]
  problems
}

# Per check: number of issues, records affected and % of the records in the tables concerned.
summarise_per_check <- function(problems, rows, dataset) {
  if (!nrow(problems)) {
    return(data.table(check_id = character(), issues = integer(), records_affected = integer(), percent_affected = numeric()))
  }
  per_check <- problems[, .(issues = sum(pmax(records, 1L))), by = check_id]

  # A record counts once per check, even with several problems (e.g. two fields).
  check_code <- match(problems$check_id, per_check$check_id)
  table_code <- match(problems$table, names(dataset$tables))
  affected <- unique(data.table(check = check_code[rows$problem], table = table_code[rows$problem], row = rows$row))
  affected_per_check <- affected[, .(records_affected = .N, records_in_tables = sum(vapply(dataset$tables[unique(table)], nrow, integer(1)))),
                                 by = check]
  per_check[, records_affected := 0L]
  per_check[affected_per_check$check, records_affected := affected_per_check$records_affected]
  per_check[, percent_affected := 0]
  per_check[affected_per_check$check, percent_affected := round(100 * affected_per_check$records_affected / affected_per_check$records_in_tables, 2)]
  per_check
}

#' The detailed issues of a QC result
#'
#' One row per issue and record, with the record's ID, file and line. Built on
#' demand from the compact storage in the result.
#'
#' @param result A result of [qc_check()].
#' @param check Optional check id(s) to keep.
#' @param severity Optional severities to keep ("error", "warning", "info").
#' @param problem Optional problem numbers (column `problem` of `result$issues_grouped`).
#'   The column `table_row` of the answer is the row of the record in its table
#'   (for [qc_record()]); `line` is the line in the file.
#' @param limit Most rows to return, most important first. Default: all.
#' @return A data.table.
#' @export
#' @examples
#' \dontrun{
#' qc_issues(result, check = "no_placeholder_values", limit = 100)
#' }
qc_issues <- function(result, check = NULL, severity = NULL, problem = NULL, limit = Inf) {
  problems <- result$issues_grouped
  # (Not problems[severity %in% severity]: inside [...] 'severity' would mean the column.)
  wanted <- rep(TRUE, nrow(problems))
  if (!is.null(check)) wanted <- wanted & problems$check_id %in% check
  if (!is.null(severity)) wanted <- wanted & problems$severity %in% severity
  if (!is.null(problem)) wanted <- wanted & problems$problem %in% problem
  problems <- problems[wanted]

  rows <- result$issue_rows[problem %in% problems$problem]
  if (is.finite(limit) && nrow(rows) > limit) {
    rows <- rows[seq_len(limit)]
    problems <- problems[problem <= max(rows$problem)]
  }
  detail <- issues_as_table(result$dataset, problems, rows)
  if (is.finite(limit)) detail <- utils::head(detail, limit)
  detail
}

# --- The verdict -----------------------------------------------------------------------
#
# One verdict for the console, the app and the Excel report:
#   errors      "Not ready for harvest": errors were found
#   incomplete  "Not complete": no errors in the checks that ran, but checks
#               that can block the harvest (severity error) did not run - e.g.
#               WoRMS or OBIS did not answer, or the check ran offline
#   warnings    "No errors": only warnings
#   clean       "No errors and no warnings"
# Checks that did not run are always named, whatever the state: a report must
# never look complete when parts of it are missing.

# Statuses of a check that did not run (its results are missing).
not_run_statuses <- c("check error", "not performed", "skipped")

#' @noRd
verdict <- function(result) {
  counts <- severity_counts(result)
  not_run <- result$checks[status %chin% not_run_statuses]
  blocking <- not_run[severity == "error"]
  checks_with <- function(s) uniqueN(result$issues_grouped[severity == s, check_id])
  number <- function(x) format(x, big.mark = ",", scientific = FALSE)

  state <- if (counts[["error"]] > 0) "errors" else if (nrow(blocking)) "incomplete" else if (counts[["warning"]] > 0) "warnings" else "clean"
  title <- c(errors = "Not ready for harvest", incomplete = "Not complete (checks did not run)",
             warnings = "No errors", clean = "No errors and no warnings")[[state]]
  text <- switch(state,
    errors = sprintf("%s errors in %d checks must be fixed first; %s warnings.", number(counts[["error"]]), checks_with("error"),
                     number(counts[["warning"]])),
    incomplete = sprintf("No errors in the checks that ran, but %d checks that can block the harvest did not run, so the dataset is not cleared yet; %s warnings.",
                         nrow(blocking), number(counts[["warning"]])),
    warnings = sprintf("%s warnings in %d checks for the data managers to look at.", number(counts[["warning"]]), checks_with("warning")),
    clean = "The dataset meets all rules that were checked.")
  if (nrow(not_run)) {
    text <- paste(text, sprintf("Did not run: %d checks (%s); their results are missing from this report.",
                                nrow(not_run), not_run_areas(not_run)))
  }
  list(state = state, title = title, text = text, not_run = not_run)
}

# "Taxonomy (WoRMS): 6, Location (incl. land): 2" - the groups of the checks that did not run.
not_run_areas <- function(not_run) {
  per_group <- not_run[, .N, by = group]
  labels <- ifelse(per_group$group %in% names(group_names), group_names[per_group$group], per_group$group)
  paste(sprintf("%s: %d", labels, per_group$N), collapse = ", ")
}

# The groups of checks (column 'group' of the rules workbook), with readable names.
group_names <- c(structure = "Files and columns", identifiers = "Identifiers", required = "Required fields",
                 vocabulary = "Vocabularies", dates = "Dates", location = "Location (incl. land)", depth = "Depth",
                 taxonomy = "Taxonomy (WoRMS)", emof = "Measurements (eMoF, NERC)", duplicates = "Duplicates",
                 other = "Other fields", dna = "DNA", outliers = "Outliers", metadata = "Metadata (EML)")

# Number of issues per severity (one per record, one per column/file/metadata problem).
severity_counts <- function(result) {
  counts <- result$issues_grouped[, .(issues = sum(pmax(records, 1L))), by = severity]
  vapply(c("error", "warning", "info"), function(s) {
    n <- counts$issues[counts$severity == s]
    if (length(n)) as.numeric(n) else 0
  }, numeric(1))
}

#' @export
print.qc_result <- function(x, ...) {
  run <- x$run
  cat("QC Tool (Beta) -", if (is.na(run$dataset_title)) run$source_name else run$dataset_title, "\n")
  cat("Source:", run$source_name, " | checked", run$checked_at, "in", run$duration_s, "s\n")
  cat("Records:", paste(sprintf("%s %s", x$tables$table, format(x$tables$records, big.mark = ",", trim = TRUE)), collapse = " | "), "\n")
  for (note in run$reading_notes) cat("Note:", note, "\n")
  if (!is.null(x$dataset$mapping)) {
    mapping <- x$dataset$mapping
    used <- mapping[!is.na(term)]
    cat(sprintf("Column mapping: %d of %d columns used: %s\n", nrow(used), nrow(mapping),
                paste(sprintf("%s -> %s%s", used$column, used$term, ifelse(used$how %in% c("exact", "your mapping"), "", paste0(" (", used$how, ")"))),
                      collapse = ", ")))
  }
  cat("\n")

  judged <- verdict(x)
  cat(toupper(judged$title), ": ", judged$text, "\n", sep = "")

  status_counts <- x$checks[, .N, by = status][order(match(status, status_levels))]
  cat("Checks:", paste(sprintf("%s %d", status_counts$status, status_counts$N), collapse = " | "), "\n")
  if (!is.null(x$lookups) && nrow(x$lookups)) {
    cat("Lookups:", paste(sprintf("%s %s %s (%s from the cache)", lookup_labels[x$lookups$service],
                                  format(x$lookups$from_cache + x$lookups$from_internet, big.mark = ","), x$lookups$what,
                                  format(x$lookups$from_cache, big.mark = ",")), collapse = " | "),
        if (isFALSE(x$run$online)) "- offline run" else "", "\n")
  }

  could_not_run <- judged$not_run
  if (nrow(could_not_run)) {
    cat("\nDid not run (their results are missing):\n")
    for (i in seq_len(nrow(could_not_run))) cat(sprintf("  %-32s %s: %s\n", could_not_run$check_id[i], could_not_run$status[i], could_not_run$reason[i]))
  }

  found <- x$checks[status == "issues found"]
  if (nrow(found)) {
    cat("\nIssues found:\n")
    for (i in seq_len(nrow(found))) {
      scope <- if (found$records_affected[i] > 0) {
        sprintf("%s records (%s%%)", format(found$records_affected[i], big.mark = ","), found$percent_affected[i])
      } else {
        "about columns, files or metadata"
      }
      cat(sprintf("  %-7s %-34s %s issues, %s\n", found$severity[i], found$check_id[i],
                  format(found$issues[i], big.mark = ","), scope))
    }
  }
  cat("\nDetails: result$issues_grouped (one row per problem), qc_issues(result) (every record);",
      "save with qc_export_xlsx() or qc_export_csv().\n")
  invisible(x)
}
