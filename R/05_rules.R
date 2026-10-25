# =============================================================================
# Rules: which checks run, how severe they are, which fields are required
#
# The rules live in one Excel workbook, inst/rules/qc_rules.xlsx, so the data
# management team can change them without touching R code:
#   sheet "Checks": one row per check; column 'severity' = error / warning / info / off;
#                   column 'settings' = every number or choice a check uses
#   sheet "Fields": one row per field; column 'requirement' = required / recommended / optional
#   sheet "Field lengths": the longest value allowed per term (EurOBIS database)
#
# The workbook is read every time qc_check() runs. Values the tool does not
# understand stop it with a message naming the sheet, row and column.
# =============================================================================

severity_levels    <- c("error", "warning", "info", "off")
requirement_levels <- c("required", "recommended", "optional")

#' Read the QC rules workbook
#'
#' Reads and validates the rules that decide which checks run, how severe
#' they are and which fields are required.
#'
#' @param path Path to a rules workbook. By default the workbook that comes
#'   with the package (`inst/rules/qc_rules.xlsx`).
#' @return A `qc_rules` object: a list with the tables `checks`, `fields` and
#'   `field_lengths`.
#' @export
#' @examples
#' rules <- qc_read_rules()
#' rules$checks[severity == "error", .(check_id, title)]
qc_read_rules <- function(path = default_rules_path()) {
  if (!file.exists(path)) stop("Rules workbook not found: ", path, call. = FALSE)

  checks <- read_rules_sheet(path, "Checks",
                             required_columns = c("check_id", "group", "title", "severity", "settings",
                                                  "what", "why", "depends_on"))
  fields <- read_rules_sheet(path, "Fields",
                             required_columns = c("term", "checked_in", "applies_to", "requirement"))

  checks[, severity := clean_choice(severity, severity_levels, path, "Checks", "severity")]
  fields[, requirement := clean_choice(requirement, requirement_levels, path, "Fields", "requirement")]
  fields[, applies_to := clean_choice(applies_to, c("all", "dna-derived"), path, "Fields", "applies_to")]

  validate_check_ids(checks, path)
  validate_dependencies(checks, path)
  validate_settings(checks, path)

  structure(list(checks = checks, fields = fields, field_lengths = read_field_lengths(path), path = normalizePath(path)),
            class = "qc_rules")
}

# Sheet "Field lengths": term, max_length (longer: error), warning_above (longer:
# warning; empty = no warning limit). An own workbook made before this sheet
# existed has none: then the limits of reference/dwc_terms.csv are used (NULL here).
read_field_lengths <- function(path) {
  if (!"Field lengths" %in% openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))) return(NULL)
  lengths <- read_rules_sheet(path, "Field lengths", required_columns = c("term", "max_length", "warning_above"))
  for (column in c("max_length", "warning_above")) {
    number <- suppressWarnings(as.integer(lengths[[column]]))
    wrong <- which(nzchar(lengths[[column]]) & (is.na(number) | number < 1))
    if (length(wrong)) {
      stop(sprintf("Rules workbook %s, sheet 'Field lengths', column '%s', row %s: not a whole number above 0",
                   basename(path), column, paste(wrong + 1, collapse = ", ")), call. = FALSE)
    }
    set(lengths, j = column, value = number)
  }
  lengths[nzchar(term), .(term, max_length, warning_above)]
}

#' @export
print.qc_rules <- function(x, ...) {
  cat("QC rules from", x$path, "\n")
  cat("Checks:", nrow(x$checks), "-", count_text(x$checks$severity, severity_levels), "\n")
  cat("Fields:", nrow(x$fields), "-", count_text(x$fields$requirement, requirement_levels), "\n")
  invisible(x)
}

default_rules_path <- function() {
  system.file("rules", "qc_rules.xlsx", package = "qctool", mustWork = TRUE)
}

# Settings of one check as a named list, e.g. "buffer_m = 3000; min_year = 1600"
# becomes list(buffer_m = 3000, min_year = 1600). Values that are not numbers
# stay text; comma-separated values become a vector.
parse_settings <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(list())
  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  names <- trimws(sub("=.*$", "", parts))
  values <- lapply(trimws(sub("^[^=]*=", "", parts)), function(value) {
    items <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    numbers <- suppressWarnings(as.numeric(items))
    if (all(!is.na(numbers))) numbers else items
  })
  names(values) <- names
  values
}

# Settings for one check, ready to pass to the check function.
settings_for <- function(rules, check_id) {
  parse_settings(rules$checks$settings[rules$checks$check_id == check_id])
}

# --- Helpers that read and validate the workbook ------------------------------

read_rules_sheet <- function(path, sheet, required_columns) {
  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
  if (!sheet %in% sheets) {
    stop("Rules workbook ", basename(path), " has no sheet '", sheet, "'.", call. = FALSE)
  }
  data <- openxlsx2::read_xlsx(path, sheet = sheet, skip_empty_rows = TRUE)
  data <- as.data.table(lapply(data, function(column) {
    column <- as.character(column)
    column[is.na(column)] <- ""
    trimws(column)
  }))
  missing <- setdiff(required_columns, names(data))
  if (length(missing)) {
    stop("Rules workbook ", basename(path), ", sheet '", sheet, "': column(s) missing: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  data
}

# Lower-case and trim a choice column, and stop when a value is not allowed.
# Excel row = data row + 1 (header).
clean_choice <- function(values, allowed, path, sheet, column) {
  values <- tolower(trimws(values))
  wrong <- which(!values %in% allowed)
  if (length(wrong)) {
    stop(sprintf("Rules workbook %s, sheet '%s', column '%s', row %s: '%s' is not one of: %s",
                 basename(path), sheet, column, paste(wrong + 1, collapse = ", "),
                 paste(unique(values[wrong]), collapse = "', '"), paste(allowed, collapse = ", ")),
         call. = FALSE)
  }
  values
}

validate_check_ids <- function(checks, path) {
  empty <- which(checks$check_id == "")
  if (length(empty)) {
    stop("Rules workbook ", basename(path), ", sheet 'Checks': empty check_id in row(s) ",
         paste(empty + 1, collapse = ", "), call. = FALSE)
  }
  doubled <- unique(checks$check_id[duplicated(checks$check_id)])
  if (length(doubled)) {
    stop("Rules workbook ", basename(path), ", sheet 'Checks': check_id used more than once: ",
         paste(doubled, collapse = ", "), call. = FALSE)
  }
}

validate_dependencies <- function(checks, path) {
  for (i in seq_len(nrow(checks))) {
    unknown <- setdiff(split_list(checks$depends_on[i]), checks$check_id)
    if (length(unknown)) {
      stop(sprintf("Rules workbook %s, sheet 'Checks', row %d (%s): depends_on names unknown check(s): %s",
                   basename(path), i + 1, checks$check_id[i], paste(unknown, collapse = ", ")), call. = FALSE)
    }
  }
  dependency_order(checks)  # stops when the dependencies form a loop
  invisible(TRUE)
}

validate_settings <- function(checks, path) {
  for (i in seq_len(nrow(checks))) {
    text <- checks$settings[i]
    if (!nzchar(text)) next
    parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
    bad <- parts[nzchar(parts) & !grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*=\\s*\\S", parts)]
    if (length(bad)) {
      stop(sprintf("Rules workbook %s, sheet 'Checks', row %d (%s), column 'settings': '%s' is not in the form name = value",
                   basename(path), i + 1, checks$check_id[i], bad[1]), call. = FALSE)
    }
  }
}

# "a; b; c" -> c("a", "b", "c")
split_list <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character())
  items <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  items[nzchar(items)]
}

# "error 58, warning 86, ..." in a fixed order
count_text <- function(values, levels) {
  counts <- table(factor(values, levels = levels))
  paste(sprintf("%s %d", names(counts), counts), collapse = ", ")
}
