# =============================================================================
# qc_check(): the one function that checks a dataset
#
#   1. read the rules workbook (which checks, how severe)
#   2. read the dataset (Darwin Core Archive, IPT/GBIF/OBIS address or table)
#   3. plan: the checks in the right order (dependencies first)
#   4. run every check; a check that fails never stops the others
#   5. build the result: run info, check status, issues (grouped and in detail)
# =============================================================================

#' Check a Darwin Core dataset against the EMODnet Biology rules
#'
#' @param input What to check:
#'   * a DwC-A zip file or unzipped archive folder
#'   * an IPT, GBIF or OBIS dataset address (`https://...`), or a link to an
#'     archive or a table, see [qc_read_url()]
#'   * a CSV, TXT or Excel table, see [qc_read_table()]
#'   * a dataset already read with one of the `qc_read_...()` functions
#' @param checks Optional: check ids to run (their dependencies are added
#'   automatically). Default: all checks in the rules workbook.
#' @param rules Optional: a rules workbook path or a [qc_read_rules()] result.
#'   Default: the workbook that comes with the package.
#' @param progress `TRUE` to show a progress bar in the console, or a function
#'   `function(done, total, title)` (used by the app).
#' @param mapping,sheet,table_type Tables only (also a table behind a web
#'   address), see [qc_read_table()].
#' @param online `TRUE` (default) to look up WoRMS, NERC and OBIS on the
#'   internet. With `FALSE`, or without a connection, only answers stored
#'   earlier in the cache are used (see [qc_clear_cache()]); checks that need
#'   more are reported as *not performed*. The default can be changed with
#'   `options(qctool.online = FALSE)`.
#' @return A `qc_result`. Print it for a summary; see [qc_issues()],
#'   [qc_export_xlsx()] and [qc_export_csv()].
#' @export
#' @examples
#' \dontrun{
#' result <- qc_check("dwca-biofun_2009-v1.9.zip")
#' result
#' qc_check("dwca-biofun_2009-v1.9.zip", online = FALSE)   # no internet: cache only
#' result$issues_grouped
#' qc_export_xlsx(result, "biofun_qc.xlsx")
#'
#' qc_check("https://ipt.vliz.be/upload/resource?r=satellitetracking-turtles-mediterranean")
#' qc_check("DataMunida.csv", mapping = c(Day = NA, Station = "locality"))
#' }
qc_check <- function(input, checks = NULL, rules = NULL, progress = FALSE,
                     mapping = NULL, sheet = NULL, table_type = "occurrence",
                     online = qc_option("qctool.online")) {
  started <- Sys.time()

  if (!inherits(rules, "qc_rules")) rules <- qc_read_rules(if (is.null(rules)) default_rules_path() else rules)
  check_functions <- package_check_functions()
  verify_check_functions(check_functions, rules)

  dataset <- if (inherits(input, "qc_dataset")) input else read_input(input, mapping, sheet, table_type)
  dataset$online <- online
  plan <- plan_checks(rules, checks)
  outcome <- run_checks(plan, dataset, rules, check_functions, progress)

  build_result(dataset, rules, plan, outcome, started)
}

# Choose the reader from the kind of input.
read_input <- function(input, mapping = NULL, sheet = NULL, table_type = "occurrence") {
  if (!is.character(input) || length(input) != 1) stop("input must be one file path, folder or address", call. = FALSE)
  if (is_url(input)) return(qc_read_url(input, mapping = mapping, sheet = sheet, table_type = table_type))
  if (dir.exists(input) || grepl("\\.zip$", input, ignore.case = TRUE)) return(qc_read_dwca(input))
  if (grepl("\\.(csv|txt|tsv|xlsx|xls)$", input, ignore.case = TRUE)) {
    return(qc_read_table(input, mapping = mapping, sheet = sheet, table_type = table_type))
  }
  stop("Cannot tell what kind of input this is: ", input,
       "\nExpected a .zip archive, an archive folder, an IPT/GBIF/OBIS address, or a .csv/.txt/.tsv/.xlsx/.xls table.",
       call. = FALSE)
}

# All check functions in the package, by check id: check_<id>() -> "<id>".
package_check_functions <- function() {
  namespace <- topenv(environment(package_check_functions))
  function_names <- ls(namespace, pattern = "^check_")
  check_functions <- Filter(is.function, mget(function_names, envir = namespace))
  names(check_functions) <- sub("^check_", "", names(check_functions))
  check_functions
}

# A check function without a row in the rules workbook would never run.
# Stop early so the two cannot drift apart unnoticed.
verify_check_functions <- function(check_functions, rules) {
  orphans <- setdiff(names(check_functions), rules$checks$check_id)
  if (length(orphans)) {
    stop("These checks exist in the code but have no row in the rules workbook (sheet 'Checks'): ",
         paste(orphans, collapse = ", "), "\nAdd a row with this check_id to ", rules$path, call. = FALSE)
  }
  invisible(TRUE)
}
