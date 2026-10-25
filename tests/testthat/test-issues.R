# Compact storage: identical problems once, with the row numbers of the records.

test_that("rows with the same value become one problem", {
  dataset <- qc_read_dwca(clean_archive())
  issues <- new_issues(dataset, "occurrence", rows = c(1L, 2L, 1L), field = "x",
                       value = c("NA", "null", "NA"), message = "placeholder")
  expect_equal(nrow(issues$problems), 2)
  expect_equal(issues$problems$value, c("NA", "null"))
  expect_equal(issues$rows$problem, c(1L, 2L, 1L))
  expect_equal(count_issues(issues), 3)
})

test_that("one value for all rows gives one problem", {
  dataset <- qc_read_dwca(clean_archive())
  issues <- new_issues(dataset, "event", rows = 1:2, field = "locality", value = "NA", message = "placeholder")
  expect_equal(nrow(issues$problems), 1)
  expect_equal(nrow(issues$rows), 2)
})

test_that("a value list of the wrong length is refused", {
  dataset <- qc_read_dwca(clean_archive())
  expect_error(new_issues(dataset, "event", rows = 1:2, value = c("a", "b", "c"), message = "x"), "one value per row")
})

test_that("problems about columns have no rows but still count", {
  dataset <- qc_read_dwca(clean_archive())
  issues <- new_issues(dataset, "event", field = c("a", "b"), message = "empty column")
  expect_equal(nrow(issues$problems), 2)
  expect_equal(nrow(issues$rows), 0)
  expect_equal(count_issues(issues), 2)
  expect_equal(issues$problems$file, c("event.txt", "event.txt"))
})

test_that("combining issues renumbers the problems", {
  dataset <- qc_read_dwca(clean_archive())
  first <- new_issues(dataset, "event", rows = 1L, message = "a")
  second <- new_issues(dataset, "occurrence", rows = 2L, message = "b")
  combined <- combine_issues(list(first, empty_issues(), second))
  expect_equal(combined$problems$problem, c(1L, 2L))
  expect_equal(combined$rows$problem, c(1L, 2L))
  expect_equal(combined$problems$message, c("a", "b"))
})

result_with_two_problems <- function() {
  folder <- make_archive(event = data.frame(id = paste0("E", 1:4), eventID = paste0("E", 1:4),
                                            locality = c("NA", "NA", "NA", "Ostend "), waterBody = c(NA, NA, NA, NA)))
  qc_check(folder)
}

test_that("the result keeps one row per problem and builds the detail on demand", {
  result <- result_with_two_problems()
  placeholder <- result$issues_grouped[check_id == "no_placeholder_values"]
  expect_equal(placeholder$records, 3)
  expect_equal(placeholder$first_line, 2)
  expect_equal(placeholder$example_records, "E1; E2; E3")

  detail <- qc_issues(result, check = "no_placeholder_values")
  expect_equal(detail$record_id, c("E1", "E2", "E3"))
  expect_equal(detail$line, 2:4)
  expect_equal(nrow(qc_issues(result)), sum(severity_counts(result)))
})

test_that("qc_issues filters by check, severity and problem number", {
  result <- result_with_two_problems()
  first <- result$issues_grouped$problem[1]
  expect_true(all(qc_issues(result, problem = first)$problem == first))
  severity_of_first <- result$issues_grouped$severity[1]
  expect_true(all(qc_issues(result, severity = severity_of_first)$severity == severity_of_first))
  expect_lt(nrow(qc_issues(result, problem = first)), nrow(qc_issues(result)))
  detail <- qc_issues(result, check = "no_placeholder_values")
  expect_equal(detail$table_row, 1:3)                     # the rows of the records in their table
})

test_that("qc_issues respects the limit, most important first", {
  result <- result_with_two_problems()
  detail <- qc_issues(result, limit = 2)
  expect_equal(nrow(detail), 2)
  expect_equal(detail$problem, sort(detail$problem))
})

test_that("issues.csv is written in chunks and compressed above the limit", {
  result <- result_with_two_problems()
  folder <- tempfile("detail_")
  dir.create(folder)
  write_issue_detail(result, folder, sep = ",", chunk_size = 1, compress_above = 2)
  expect_true(file.exists(file.path(folder, "issues.csv.gz")))
  lines <- readLines(gzfile(file.path(folder, "issues.csv.gz")))
  expect_equal(length(lines) - 1, sum(severity_counts(result)))   # one header line
  expect_match(lines[1], "^problem,check_id,severity")
})
