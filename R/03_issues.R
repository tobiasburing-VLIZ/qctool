# =============================================================================
# How a check reports problems
#
# Every check returns its issues made with new_issues() (or empty_issues()
# when there is nothing to report).
#
# COMPACT STORAGE. A dataset like FIFD can have 40 million issues that are
# really a handful of identical problems ("NA" text in measurementUnitID in
# 24 million records). So issues are stored in two tables:
#   problems  one row per identical problem: table, field, value, message,
#             suggestion, file. Read by people: "NA instead of empty: 24,459,540 records"
#   rows      one row per affected record: problem number + row number in the
#             table. Only two integers per record.
# The full detail per record (ID, file, line) is built only when it is asked
# for: qc_issues(), the exports and (later) the app. See R/04_result.R.
#
# RULE: no issue without a pointer. From the row number, every issue leads to
# exactly one record (docs/design.md, section 4.7):
#   1. the record's own ID   eventID / occurrenceID / measurementID, or for
#                            eMoF rows without measurementID: occurrenceID (or
#                            the event link) in record_id + the measurementType
#                            in the column measurement_type
#   2. file and line         the line in the original file; works even when
#                            the ID is empty or duplicated
# Issues about a whole column, file or the metadata have no rows, but say
# which column / file / metadata element they are about.
#
# Write the message WITHOUT the record's value (put that in 'value'), so that
# identical problems are recognised as one: "AphiaID not found: 2,300 records".
# =============================================================================

empty_problems <- function() {
  data.table(problem = integer(), table = character(), file = character(), field = character(),
             value = character(), message = character(), suggestion = character(), severity_hint = character())
}

empty_issues <- function() {
  structure(list(problems = empty_problems(), rows = data.table(problem = integer(), row = integer())),
            class = "qc_issues")
}

#' Report problems found by a check
#'
#' Used inside check functions. Arguments can be one value for all rows, or
#' one value per row; rows with the same field, value, message, suggestion and
#' severity hint are stored together as one problem.
#'
#' @param dataset The dataset being checked.
#' @param table Table name ("event", "occurrence", "emof", ...), or "archive" / "eml".
#' @param rows Row numbers in that table that have the problem. Leave `NULL`
#'   for a problem about a whole column, file or the metadata.
#' @param field The field(s) concerned, e.g. "decimalLatitude".
#' @param value The value(s) that caused the problem.
#' @param message What is wrong, in plain words, without the value itself.
#' @param suggestion Optional proposed fix (never applied automatically).
#' @param severity_hint Optional "warning" or "info" to LOWER the severity of
#'   these issues below the severity in the rules workbook.
#' @param file Optional file name for problems that are not in a table (e.g. meta.xml).
#' @keywords internal
new_issues <- function(dataset, table, rows = NULL, field = NA_character_, value = NA_character_,
                       message, suggestion = NA_character_, severity_hint = NA_character_, file = NULL) {
  if (!is.null(rows) && !length(rows)) return(empty_issues())
  if (is.null(file)) file <- if (table %in% names(dataset$tables)) table_file(dataset, table) else NA_character_

  details <- list(file = file, field = field, value = value, message = message,
                  suggestion = suggestion, severity_hint = severity_hint)
  details <- lapply(details, as.character)

  # Problems about a column, file or the metadata: one problem each, no rows.
  if (is.null(rows)) {
    n <- max(lengths(details))
    problems <- as.data.table(lapply(details, rep_len, length.out = n))
    set(problems, j = "problem", value = seq_len(n))
    set(problems, j = "table", value = table)
    return(issues_from(problems, data.table(problem = integer(), row = integer())))
  }

  # Problems about records: rows with the same details form one problem.
  varying <- names(details)[lengths(details) > 1]
  for (name in varying) {
    if (length(details[[name]]) != length(rows)) {
      stop("new_issues(): '", name, "' must have one value, or one value per row (", length(rows), ")")
    }
  }
  if (!length(varying)) {
    problems <- as.data.table(details)
    set(problems, j = "problem", value = 1L)
    set(problems, j = "table", value = table)
    return(issues_from(problems, data.table(problem = 1L, row = as.integer(rows))))
  }

  per_row <- as.data.table(details[varying])
  per_row[, problem := .GRP, by = varying]
  problems <- unique(per_row, by = "problem")
  for (name in setdiff(names(details), varying)) set(problems, j = name, value = details[[name]])
  set(problems, j = "table", value = table)
  issues_from(problems, data.table(problem = per_row$problem, row = as.integer(rows)))
}

# Put the two tables together with the columns in the standard order.
issues_from <- function(problems, rows) {
  setcolorder(problems, names(empty_problems()))
  structure(list(problems = problems, rows = rows), class = "qc_issues")
}

# Combine the issues of several tables or columns into one qc_issues object.
# Problem numbers are renumbered so they stay unique.
combine_issues <- function(issue_list) {
  issue_list <- Filter(function(x) !is.null(x) && nrow(x$problems) > 0, issue_list)
  if (!length(issue_list)) return(empty_issues())

  problem_counts <- vapply(issue_list, function(x) nrow(x$problems), integer(1))
  row_counts <- vapply(issue_list, function(x) nrow(x$rows), integer(1))
  offsets <- c(0L, cumsum(problem_counts)[-length(problem_counts)])

  problems <- rbindlist(lapply(issue_list, `[[`, "problems"), use.names = TRUE, fill = TRUE)
  rows <- rbindlist(lapply(issue_list, `[[`, "rows"), use.names = TRUE)
  problems[, problem := problem + rep(offsets, problem_counts)]
  rows[, problem := problem + rep(offsets, row_counts)]
  structure(list(problems = problems, rows = rows), class = "qc_issues")
}

# Number of issues: one per affected record, one per column/file/metadata problem.
count_issues <- function(issues) {
  nrow(issues$rows) + sum(!issues$problems$problem %in% issues$rows$problem)
}

# --- From row numbers to records ----------------------------------------------------

# The two pointers for rows of one table: the record's ID and file + line.
# Only existing text values are copied, no new text is built: with tens of
# millions of issues (FIFD) building text such as "O1 | abundance" for every
# row costs minutes and gigabytes.
record_pointer <- function(dataset, table_name, rows) {
  data <- dataset$tables[[table_name]]
  n <- length(rows)
  record_id <- rep(NA_character_, n)
  record_id_field <- rep(NA_character_, n)
  core_link <- data$.coreid[rows]
  core_id_name <- unname(c(event = "eventID", occurrence = "occurrenceID")[dataset$core])
  if (is.na(core_id_name)) core_id_name <- "core id"

  # 1. The record's own ID
  own_id_field <- c(event = "eventID", occurrence = "occurrenceID", emof = "measurementID",
                    mof = "measurementID")[table_name]
  if (!is.na(own_id_field) && own_id_field %in% names(data)) {
    record_id <- data[[own_id_field]][rows]
    record_id_field[!is.na(record_id)] <- own_id_field
  }

  # 2. eMoF rows without measurementID: the occurrence they belong to. Together
  #    with measurement_type (own column) this identifies the eMoF record.
  measurement_type <- rep(NA_character_, n)
  if (table_name %in% c("emof", "mof")) {
    if ("measurementType" %in% names(data)) measurement_type <- data$measurementType[rows]
    missing <- which(is.na(record_id))
    if ("occurrenceID" %in% names(data) && length(missing)) {
      occurrence_id <- data$occurrenceID[rows[missing]]
      found <- !is.na(occurrence_id)
      record_id[missing[found]] <- occurrence_id[found]
      record_id_field[missing[found]] <- "occurrenceID"
    }
  }

  # 3. Anything else: the core record it belongs to
  missing <- which(is.na(record_id) & !is.na(core_link))
  record_id[missing] <- core_link[missing]
  record_id_field[missing] <- paste(core_id_name, "(core)")

  is_core <- identical(table_name, dataset$core)
  data.table(file = table_file(dataset, table_name), line = data$.line[rows],
             record_id_field = record_id_field, record_id = record_id, measurement_type = measurement_type,
             core_id = if (is_core) NA_character_ else core_link)
}

# Problem number + record pointer (file, line, ID) for the given issue rows.
# include_rowless: also one row for each problem without records (about a
# column, file or the metadata).
issue_pointers <- function(dataset, problems, rows, include_rowless = TRUE) {
  pieces <- list()
  problem_table <- problems$table[match(rows$problem, problems$problem)]
  for (table_name in unique(problem_table)) {
    in_table <- rows[problem_table == table_name]
    pieces[[table_name]] <- data.table(problem = in_table$problem, record_pointer(dataset, table_name, in_table$row),
                                       table_row = in_table$row)
  }
  if (include_rowless) {
    rowless <- if ("records" %in% names(problems)) problems$records == 0 else !problems$problem %in% rows$problem
    if (any(rowless)) {
      pieces[["rowless"]] <- problems[rowless, .(problem, file, line = NA_integer_, record_id_field = NA_character_,
                                                 record_id = NA_character_, measurement_type = NA_character_,
                                                 core_id = NA_character_, table_row = NA_integer_)]
    }
  }
  if (!length(pieces)) {
    return(data.table(problem = integer(), file = character(), line = integer(), record_id_field = character(),
                      record_id = character(), measurement_type = character(), core_id = character(),
                      table_row = integer()))
  }
  rbindlist(pieces, use.names = TRUE)
}

# One row per issue with all details: problem details + record pointer.
# Used by qc_issues(), the exports and the tests.
issues_as_table <- function(dataset, problems, rows, include_rowless = TRUE) {
  detail <- issue_pointers(dataset, problems, rows, include_rowless)
  details_of_problem <- setdiff(names(problems), c("problem", "file", "records", "first_line", "example_records"))
  detail <- cbind(detail, problems[match(detail$problem, problems$problem), details_of_problem, with = FALSE])
  setcolorder(detail, intersect(c("problem", "check_id", "severity", "table", "file", "line", "record_id_field",
                                  "record_id", "measurement_type", "core_id", "field", "value", "message",
                                  "suggestion", "severity_hint"), names(detail)))
  detail[order(problem, line)]
}

# --- Stopping a check politely ------------------------------------------------------

#' Stop a check because there is nothing to check
#'
#' The check gets the status "not applicable" with this reason. Use inside
#' check functions, e.g. `skip_check("no event table")`.
#' @param reason Short explanation shown in the report.
#' @keywords internal
skip_check <- function(reason) {
  stop(structure(class = c("qc_not_applicable", "error", "condition"),
                 list(message = reason, call = NULL)))
}

# The check needs something (e.g. internet) that is not available: status "not performed".
not_performed <- function(reason) {
  stop(structure(class = c("qc_not_performed", "error", "condition"),
                 list(message = reason, call = NULL)))
}

require_table <- function(dataset, table) {
  if (!table %in% names(dataset$tables)) skip_check(sprintf("no %s table", table))
  invisible(dataset$tables[[table]])
}

require_fields <- function(dataset, table, fields) {
  data <- require_table(dataset, table)
  missing <- setdiff(fields, names(data))
  if (length(missing)) skip_check(sprintf("no %s column in the %s table", paste(missing, collapse = " / "), table))
  invisible(data)
}
