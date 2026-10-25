# Fake rules and check functions to test the engine on its own.
fake_rules <- function(severity = c(first = "error", second = "warning", third = "warning"),
                       depends_on = c(first = "", second = "first", third = "")) {
  checks <- data.table(check_id = names(severity), group = "test", title = names(severity),
                       severity = unname(severity), settings = "", what = "", why = "",
                       depends_on = unname(depends_on[names(severity)]), milestone = "4a")
  structure(list(checks = checks, fields = data.table(), path = "fake"), class = "qc_rules")
}

one_issue <- function(dataset, settings) {
  new_issues(dataset, "event", rows = 1L, field = "eventID", message = "test problem")
}
no_issue <- function(dataset, settings) empty_issues()
broken <- function(dataset, settings) stop("something unexpected")
nothing_to_check <- function(dataset, settings) skip_check("no such table")

run_fake <- function(functions, rules = fake_rules()) {
  dataset <- qc_read_dwca(clean_archive())
  outcome <- run_checks(plan_checks(rules), dataset, rules, functions)
  outcome$detail <- issues_as_table(dataset, outcome$problems, outcome$rows)
  outcome
}

test_that("checks run after the checks they depend on", {
  rules <- fake_rules(severity = c(second = "error", first = "error", third = "error"))
  expect_equal(plan_checks(rules)$check_id, c("first", "second", "third"))
})

test_that("asking for one check adds its dependencies", {
  plan <- plan_checks(fake_rules(), only = "second")
  expect_equal(plan$check_id, c("first", "second"))
  expect_equal(plan$added_as_dependency, c(TRUE, FALSE))
})

test_that("a failing check does not stop the others", {
  outcome <- run_fake(list(first = broken, second = one_issue, third = one_issue))
  expect_equal(outcome$checks$status, c("check error", "skipped", "issues found"))
  expect_match(outcome$checks$reason[1], "something unexpected")
  expect_match(outcome$checks$reason[2], "needs first")
})

test_that("issues found by a dependency do NOT stop the dependent check", {
  outcome <- run_fake(list(first = one_issue, second = one_issue, third = no_issue))
  expect_equal(outcome$checks$status, c("issues found", "issues found", "passed"))
})

test_that("not applicable, not implemented and switched off are reported as such", {
  rules <- fake_rules(severity = c(first = "error", second = "warning", third = "off"))
  outcome <- run_fake(list(first = nothing_to_check, third = one_issue), rules)
  expect_equal(outcome$checks$status, c("not applicable", "not yet implemented", "switched off"))
})

test_that("severity comes from the rules; a hint can lower it but not raise it", {
  lowered <- function(dataset, settings) {
    new_issues(dataset, "event", rows = 1:2, message = "x", severity_hint = c("warning", NA))
  }
  raised <- function(dataset, settings) new_issues(dataset, "event", rows = 1L, message = "x", severity_hint = "error")
  outcome <- run_fake(list(first = lowered, second = no_issue, third = raised))
  expect_equal(outcome$detail[check_id == "first", severity], c("warning", "error"))
  expect_equal(outcome$detail[check_id == "third", severity], "warning")
})

test_that("every issue points to a record: its ID, file and line", {
  outcome <- run_fake(list(first = one_issue, second = no_issue, third = no_issue))
  issue <- outcome$detail[1]
  expect_equal(issue$record_id, "E1")
  expect_equal(issue$record_id_field, "eventID")
  expect_equal(issue$file, "event.txt")
  expect_equal(issue$line, 2L)
})

test_that("eMoF rows without measurementID are identified by occurrenceID + measurementType", {
  dataset <- qc_read_dwca(clean_archive())
  pointer <- record_pointer(dataset, "emof", 2L)
  expect_equal(pointer$record_id, "O2")
  expect_equal(pointer$record_id_field, "occurrenceID")
  expect_equal(pointer$measurement_type, "abundance")
  expect_equal(pointer$core_id, "E2")
})

test_that("records without an own ID point to their core record", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"),
                                       emof = data.frame(id = "E1", measurementType = "depth")))
  pointer <- record_pointer(dataset, "emof", 1L)
  expect_equal(pointer$record_id, "E1")
  expect_equal(pointer$record_id_field, "eventID (core)")
})

test_that("qc_check runs the package rules end to end", {
  result <- qc_check(clean_archive())
  expect_s3_class(result, "qc_result")
  expect_true(all(result$checks$status %in% status_levels))
  expect_true(all(c("run", "tables", "checks", "issues_grouped", "issue_rows", "dataset") %in% names(result)))
  expect_output(print(result), "QC Tool \\(Beta\\)")
})
