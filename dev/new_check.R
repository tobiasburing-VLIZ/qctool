# =============================================================================
# Start a new check: prints the three pieces a check needs, ready to copy.
#
#   source("dev/new_check.R")
#   new_check("depth_not_negative", group = "depth", table = "event",
#             field = "minimumDepthInMeters",
#             title = "Depth is not negative",
#             what = "minimumDepthInMeters is 0 or more.",
#             why = "A negative depth is a height above the sea surface: usually a sign error.")
#
# Then (the manual, section "Adding a check", explains each step):
#   1. Add the printed row to the sheet Checks of inst/rules/qc_rules.xlsx.
#   2. Paste the function into the printed R/checks_NN_<group>.R file and
#      write the test in it ("what is a problem here?").
#   3. Paste the test into the printed tests/testthat file and make it fail
#      and pass for the right reasons.
#   4. devtools::load_all(); devtools::test()
#   5. Rscript dev/stress_app_functions.R   (all test datasets, no crash)
# The app shows the new check by itself (Help > Checks, Overview, Data issues).
# =============================================================================

new_check <- function(check_id, group, table = "occurrence", field = "scientificName", title = "Title for people",
                      what = "What the check looks at, in one sentence.", why = "Why this matters for the data.",
                      severity = "warning") {
  stopifnot(grepl("^[a-z][a-z0-9_]*$", check_id))
  files <- c(structure = "checks_01_structure.R", identifiers = "checks_02_identifiers.R", required = "checks_03_required.R",
             vocabulary = "checks_04_vocabulary.R", dates = "checks_05_dates.R", location = "checks_06_location.R",
             depth = "checks_07_depth.R", taxonomy = "checks_08_taxonomy.R", emof = "checks_09_emof.R",
             duplicates = "checks_10_duplicates.R", other = "checks_11_other.R", metadata = "checks_12_metadata.R",
             outliers = "checks_13_outliers.R", dna = "checks_14_dna.R")
  tests <- c(structure = "test-checks_01_structure.R", identifiers = "test-checks_02_identifiers.R",
             required = "test-checks_03_required.R", vocabulary = "test-checks_04_vocabulary.R", dates = "test-checks_05_dates.R",
             location = "test-checks_06_location.R", depth = "test-checks_07_08_depth_taxonomy.R",
             taxonomy = "test-checks_07_08_depth_taxonomy.R", emof = "test-checks_09_emof.R")
  if (!group %in% names(files)) stop("group must be one of: ", paste(names(files), collapse = ", "))
  test_file <- if (group %in% names(tests)) tests[[group]] else "test-checks_10_to_14.R"

  cat("\n== 1. Row for inst/rules/qc_rules.xlsx, sheet 'Checks' ===================\n")
  cat(sprintf("check_id: %s\ngroup: %s\ntitle: %s\nseverity: %s\nsettings: (empty, or e.g. min_year = 1600)\nwhat: %s\nwhy: %s\n",
              check_id, group, title, severity, what, why))
  cat(sprintf("depends_on: (empty, or a check that must run first)\ntables: %s\nfields: %s\nlookup: none\nmilestone: 6\n", table, field))

  cat(sprintf("\n== 2. Function for R/%s =========================================\n", files[[group]]))
  cat(sprintf('
# -----------------------------------------------------------------------------
# CHECK  %1$s
# What:  %2$s
# Why:   %3$s
# -----------------------------------------------------------------------------
check_%1$s <- function(dataset, settings) {
  data <- require_fields(dataset, "%4$s", "%5$s")      # "not applicable" when the table or column is missing
  values <- data$%5$s
  # rows_where() tests each DIFFERENT value once: fast on millions of records.
  rows <- rows_where(values, function(v) FALSE)            # <- write the test: TRUE = a problem
  new_issues(dataset, "%4$s", rows, field = "%5$s", value = values[rows],
             message = "%6$s",
             suggestion = NA_character_)                   # optional: what it should be
}
', check_id, what, why, table, field, title))

  cat(sprintf("\n== 3. Test for tests/testthat/%s ======================================\n", test_file))
  cat(sprintf('
test_that("%1$s finds the problem and leaves good records alone", {
  dataset <- qc_read_dwca(make_archive(%2$s = data.frame(
    id = c("R1", "R2"), %2$sID = c("R1", "R2"), %3$s = c("a good value", "a bad value"))))
  issues <- run_check("%1$s", dataset)
  expect_equal(issues$record_id, "R2")
})
', check_id, table, field))

  cat("\n== Then ===================================================================\n")
  cat("devtools::load_all(); devtools::test()   and   Rscript dev/stress_app_functions.R\n")
  cat("Watch out (manual, 'Adding a check'): no helper may start with check_; inside data[...] a name means\n")
  cat("the column first; never run trimws()/tolower()/grepl() over a whole column: use rows_where() or per_value().\n")
  invisible(NULL)
}
