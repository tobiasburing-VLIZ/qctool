# =============================================================================
# The check catalogue: every check of the rules workbook, described for people
#
#   qc_checks()                  all checks
#   qc_checks("NERC label")      the checks whose description has these words
#
# The same catalogue is the "Checks" screen of the app and the appendix of the
# manual (dev/write_check_catalogue.R writes inst/manual/checks.md).
# =============================================================================

#' The check catalogue: what each check does, and how
#'
#' @param search Optional words; only checks whose title, id, description,
#'   fields, settings, value sources or notes contain all of them (any case) are returned.
#' @param rules Optional rules (see [qc_read_rules()]); default: the workbook
#'   that comes with the package.
#' @return A data.table, one row per check: check_id, group, title, severity,
#'   implemented, internet (which online service it uses, if any), what, why,
#'   other_standards, tables, fields, settings, depends_on, reference_list,
#'   notes and code (the file and function of the check).
#' @export
#' @examples
#' \dontrun{
#' qc_checks("NERC label")    # is there a check that compares measurementType with the NERC label?
#' qc_checks("land")
#' }
qc_checks <- function(search = NULL, rules = NULL) {
  find_checks(catalogue_of_checks(rules), search)
}

catalogue_of_checks <- function(rules = NULL) {
  if (is.null(rules)) rules <- qc_read_rules()
  checks <- copy(rules$checks)
  functions <- package_check_functions()
  column <- function(name) if (name %in% names(checks)) as.character(checks[[name]]) else rep(NA_character_, nrow(checks))

  catalogue <- data.table(
    check_id = checks$check_id, group = checks$group, title = checks$title, severity = checks$severity,
    implemented = ifelse(checks$check_id %in% names(functions), "yes", paste("planned, milestone", column("milestone"))),
    internet = online_service_of(column("lookup")),
    what = column("what"), why = column("why"), other_standards = column("other_standards"),
    tables = column("tables"), fields = column("fields"), settings = column("settings"),
    depends_on = column("depends_on"), reference_list = column("reference_list"), notes = column("notes"),
    code = vapply(checks$check_id, function(id) code_location(id, functions), character(1)))
  catalogue[order(group, title)]
}

# The checks whose title, id, description, fields, settings, value sources or
# notes contain all the words.
find_checks <- function(catalogue, search) {
  words <- tolower(unlist(strsplit(paste(search, collapse = " "), "\\s+")))
  words <- words[nzchar(words)]
  if (!length(words)) return(catalogue)
  text <- tolower(do.call(paste, c(as.list(catalogue[, .(check_id, title, what, why, fields, tables, notes, group,
                                                          settings, reference_list)]), sep = " ")))
  catalogue[Reduce(`&`, lapply(words, function(word) grepl(word, text, fixed = TRUE)))]
}

# The online service a check uses, in words ("" when none).
online_service_of <- function(lookup) {
  services <- c(worms = "WoRMS", nerc = "NERC Vocabulary Server", land = "OBIS (coast; offline: coarse coastline)",
                bathymetry = "OBIS (sea floor depth)", web = "web", previous_run = "previous run (history)", database = "EurOBIS database")
  vapply(strsplit(ifelse(is.na(lookup), "", lookup), ";\\s*"), function(parts) {
    parts <- parts[parts %in% names(services)]
    paste(services[parts], collapse = " + ")
  }, character(1))
}

# "R/checks_09_emof_nerc.R, function check_measurementtype_matches_typeid()"
code_location <- function(check_id, functions) {
  f <- functions[[check_id]]
  if (is.null(f)) return("not written yet")
  file <- utils::getSrcFilename(f)
  if (length(file) && !is.na(file) && nzchar(file)) {
    file <- paste0("R/", file)
  } else {
    # An installed package keeps no source file names: use the list that
    # dev/write_check_catalogue.R writes.
    known <- tryCatch(reference_table("check_code_files.csv"), error = function(e) NULL)
    file <- if (!is.null(known) && check_id %in% known$check_id) known$file[match(check_id, known$check_id)] else "R/checks_*.R"
  }
  sprintf("%s, function check_%s()", file, check_id)
}
