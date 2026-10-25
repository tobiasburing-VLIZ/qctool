result_with_issues <- function() {
  folder <- make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                            locality = c("Ostend ", "NA")))
  qc_check(folder)
}

test_that("the Excel export has the expected sheets and all issues", {
  result <- result_with_issues()
  file <- tempfile(fileext = ".xlsx")
  qc_export_xlsx(result, file)

  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(file))
  expect_equal(unname(sheets), c("Summary", "Checks", "Issues grouped", "Issues", "About the checks"))
  issues <- openxlsx2::read_xlsx(file, sheet = "Issues")
  expect_equal(nrow(issues), nrow(qc_issues(result)))
  expect_true(all(c("record_id", "file", "line") %in% names(issues)))
})

test_that("above the Excel limit the detail is cut, and the Summary says so", {
  result <- result_with_issues()
  expect_gt(nrow(qc_issues(result)), 1)
  file <- tempfile(fileext = ".xlsx")
  qc_export_xlsx(result, file, max_issue_rows = 1)

  expect_equal(nrow(openxlsx2::read_xlsx(file, sheet = "Issues")), 1)
  expect_equal(nrow(openxlsx2::read_xlsx(file, sheet = "Issues grouped")), 1)
  summary <- openxlsx2::read_xlsx(file, sheet = "Summary", col_names = FALSE)
  expect_match(summary[[1]][3], "'Issues' shows the 1 most important of")
  expect_match(summary[[1]][3], "issues_grouped.csv and issues.csv")
})

test_that("the CSV export writes four files that read back completely", {
  result <- result_with_issues()
  folder <- tempfile("csv_")
  qc_export_csv(result, folder, sep = ";")

  expect_setequal(list.files(folder), c("checks.csv", "issues.csv", "issues_grouped.csv", "run.json"))
  issues <- fread(file.path(folder, "issues.csv"), sep = ";")
  expect_equal(nrow(issues), nrow(qc_issues(result)))
  expect_equal(jsonlite::fromJSON(file.path(folder, "run.json"))$tool, "QC Tool (Beta)")
})

test_that("the JSON export contains all parts", {
  result <- result_with_issues()
  file <- tempfile(fileext = ".json")
  qc_export_json(result, file)
  content <- jsonlite::fromJSON(file)
  expect_true(all(c("run", "tables", "checks", "issues_grouped", "issues") %in% names(content)))
})
