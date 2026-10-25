# =============================================================================
# The tables of a checked dataset, to look at the data itself (app: Explore >
# Tables): what each table is and how its records are linked, its columns,
# and rows found by a search over the whole table (not only the rows shown).
#
#   qc_tables(result)                    one row per table: role, records, links
#   qc_table_columns(result, table)      one row per column: how often filled, examples
#   qc_find_rows(result, table, ...)     the first rows, or the rows where a column
#                                        is / contains a value, is empty, or a line number
# =============================================================================

#' The tables of a checked dataset and how they are linked
#'
#' @param result A [qc_check()] result.
#' @return One row per table: table, file, role (core or extension), records,
#'   columns, and how its records are linked to the other tables.
#' @export
qc_tables <- function(result) {
  dataset <- result$dataset
  info <- result$tables
  links <- vapply(info$table, function(t) table_links(dataset, t), character(1))
  data.table(table = table_label(info$table), file = info$file, role = ifelse(info$core, "core", "extension"),
             records = info$records, columns = info$columns, `linked to` = unname(links))
}

# How the records of one table are linked, in words with numbers.
table_links <- function(dataset, table_name) {
  data <- dataset$tables[[table_name]]
  if (is.null(data)) return("")
  count <- function(n) format(n, big.mark = ",")
  if (table_name == "event") {
    children <- if ("parentEventID" %in% names(data)) sum(!is.na(data$parentEventID) & nzchar(data$parentEventID)) else 0
    return(if (children > 0) sprintf("%s events have a parent event (parentEventID); %s are top-level events", count(children),
                                     count(nrow(data) - children))
           else "no parent events: all events are top-level")
  }
  if (table_name == "occurrence") {
    if (identical(dataset$core, "occurrence")) return("the core: other tables point to its records")
    to_event <- link_rows(dataset, "occurrence", "event", "eventID")
    return(sprintf("%s of %s occurrences are linked to an event", count(sum(!is.na(to_event))), count(nrow(data))))
  }
  if (identical(table_name, emof_table_name(dataset))) {
    to_occurrence <- if ("occurrence" %in% names(dataset$tables)) !is.na(emof_occurrence_row(dataset)) else rep(FALSE, nrow(data))
    to_event <- if ("event" %in% names(dataset$tables)) !is.na(emof_event_row(dataset)) else rep(FALSE, nrow(data))
    return(sprintf("%s measurements of an occurrence, %s of an event only, %s not linked",
                   count(sum(to_occurrence)), count(sum(to_event & !to_occurrence)), count(sum(!to_event & !to_occurrence))))
  }
  if (identical(dataset$core, table_name)) return("the core: other tables point to its records")
  core <- dataset$tables[[dataset$core]]
  if (is.null(core) || all(is.na(data$.coreid))) return("")
  sprintf("%s of %s records are linked to the %s core", count(sum(data$.coreid %in% core$.coreid)), count(nrow(data)), dataset$core)
}

#' The columns of one table: how often each is filled, with examples
#'
#' @param result A [qc_check()] result.
#' @param table "event", "occurrence", "emof", ...
#' @return One row per column: column (the Darwin Core term), filled (number of
#'   records with a value), filled_percent, examples (up to 3 different values).
#' @export
qc_table_columns <- function(result, table) {
  data <- result$dataset$tables[[table]]
  if (is.null(data)) stop("No table '", table, "'")
  columns <- data_columns(data)
  rbindlist(lapply(columns, function(column) {
    values <- data[[column]]
    filled <- sum(!is.na(values) & nzchar(values))
    first_values <- utils::head(values[!is.na(values) & nzchar(values)], 1000)
    data.table(column = column, filled = filled, filled_percent = if (nrow(data)) round(100 * filled / nrow(data), 1) else 0,
               examples = shorten(paste(utils::head(unique(first_values), 3), collapse = " | "), 120))
  }))
}

#' Rows of one table: the first rows, or those a search finds in the whole table
#'
#' @param result A [qc_check()] result.
#' @param table "event", "occurrence", "emof", ...
#' @param column The column to search in (a Darwin Core term).
#' @param value The value to look for (text; for `how = "line"` a line number).
#' @param how "first" (the first rows), "is" (exactly this value), "contains"
#'   (this text anywhere, ignoring capitals), "empty", or "line" (this line of the file).
#' @param limit At most this many rows are returned.
#' @return A list: `rows` (line in the file, then all columns) and `total`
#'   (how many rows were found).
#' @export
qc_find_rows <- function(result, table, column = NULL, value = "", how = c("first", "is", "contains", "empty", "line"), limit = 1000) {
  how <- match.arg(how)
  data <- result$dataset$tables[[table]]
  if (is.null(data)) stop("No table '", table, "'")
  if (how %in% c("is", "contains", "empty") && !isTRUE(column %in% names(data))) stop("No column '", column, "' in the ", table, " table")
  found <- switch(how,
    first    = seq_len(nrow(data)),
    is       = which(data[[column]] == value),
    contains = rows_where(data[[column]], function(v) grepl(tolower(value), tolower(utf8_safe(v)), fixed = TRUE)),
    empty    = which(is.na(data[[column]]) | !nzchar(trimws(data[[column]]))),
    line     = which(data$.line == suppressWarnings(as.integer(value))))
  shown <- utils::head(found, limit)
  columns <- data_columns(data)
  rows <- cbind(data.table(line = data$.line[shown]), link_values(result$dataset, table, shown), data[shown, ..columns])
  list(rows = rows, total = length(found))
}
