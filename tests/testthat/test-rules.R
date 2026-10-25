test_that("the rules workbook of the package is valid", {
  rules <- qc_read_rules()
  expect_s3_class(rules, "qc_rules")
  expect_true(all(rules$checks$severity %in% c("error", "warning", "info", "off")))
  expect_true(all(rules$fields$requirement %in% c("required", "recommended", "optional")))
  expect_false(anyDuplicated(rules$checks$check_id) > 0)
})

test_that("every check function in the code has a row in the rules workbook", {
  expect_silent(verify_check_functions(package_check_functions(), qc_read_rules()))
})

# A small rules workbook for testing wrong values.
write_rules <- function(checks, fields = data.frame(term = "eventID", checked_in = "event", applies_to = "all",
                                                     requirement = "required")) {
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Checks")$add_data("Checks", checks)
  wb$add_worksheet("Fields")$add_data("Fields", fields)
  wb$save(path)
  path
}

check_rows <- function(...) {
  data.frame(check_id = c("a", "b"), group = "test", title = c("A", "B"), severity = c("error", "warning"),
             settings = "", what = "", why = "", depends_on = c("", "a"), ...)
}

test_that("severity is read without regard to capitals or spaces", {
  checks <- check_rows()
  checks$severity <- c(" Error ", "WARNING")
  rules <- qc_read_rules(write_rules(checks))
  expect_equal(rules$checks$severity, c("error", "warning"))
})

test_that("a wrong severity stops with the sheet, column and Excel row", {
  checks <- check_rows()
  checks$severity[2] <- "serious"
  expect_error(qc_read_rules(write_rules(checks)), "sheet 'Checks', column 'severity', row 3: 'serious'")
})

test_that("unknown dependencies and loops are refused", {
  checks <- check_rows()
  checks$depends_on[2] <- "zzz"
  expect_error(qc_read_rules(write_rules(checks)), "unknown check\\(s\\): zzz")

  checks <- check_rows()
  checks$depends_on <- c("b", "a")
  expect_error(qc_read_rules(write_rules(checks)), "form a loop")
})

test_that("field lengths come from the sheet 'Field lengths'; an older workbook without it still works", {
  lengths <- qc_read_rules()$field_lengths
  expect_equal(lengths[term == "institutionCode", max_length], 50L)
  expect_equal(unlist(lengths[term == "associatedOccurrences", .(max_length, warning_above)], use.names = FALSE), c(1000L, 350L))
  expect_null(qc_read_rules(write_rules(check_rows()))$field_lengths)

  path <- write_rules(check_rows())
  wb <- openxlsx2::wb_load(path)
  wb$add_worksheet("Field lengths")$add_data("Field lengths", data.frame(term = "institutionCode", max_length = "fifty", warning_above = ""))
  wb$save(path)
  expect_error(qc_read_rules(path), "sheet 'Field lengths', column 'max_length', row 2")
})

test_that("every setting a check reads is written in the workbook, with its value", {
  code_folder <- test_path("..", "..", "R")
  skip_if_not(dir.exists(code_folder))   # the code files exist in the source folder only
  rules <- qc_read_rules()$checks
  for (file in list.files(code_folder, pattern = "^checks_.*\\.R$", full.names = TRUE)) {
    code <- paste(readLines(file, warn = FALSE), collapse = "\n")
    functions <- regmatches(code, gregexpr("check_[a-z0-9_]+ <- function\\(dataset, settings\\) \\{.*?\n\\}", code))[[1]]
    for (f in functions) {
      check_id <- sub("^check_([a-z0-9_]+) <- .*", "\\1", f)
      used <- sub(".*\"(.*)\"", "\\1", regmatches(f, gregexpr("setting\\(settings, \"[a-z_]+\"", f))[[1]])
      written <- names(parse_settings(rules$settings[match(check_id, rules$check_id)]))
      expect_true(all(used %in% written), info = sprintf("%s: %s", check_id, paste(setdiff(used, written), collapse = ", ")))
    }
  }
})

test_that("the rules workbook names no people and no internal tickets", {
  rules <- qc_read_rules()
  text <- c(unlist(rules$checks), unlist(rules$fields))
  expect_false(any(grepl("jira|eurobis-[0-9]{3}|cyril", text, ignore.case = TRUE)))
  expect_false("source" %in% names(rules$checks))
})

test_that("settings are read as name = value", {
  expect_equal(parse_settings("buffer_m = 3000; min_year = 1600"), list(buffer_m = 3000, min_year = 1600))
  expect_equal(parse_settings("suspicious_dates = 1900-01-01, 1970-01-01"),
               list(suspicious_dates = c("1900-01-01", "1970-01-01")))
  expect_equal(parse_settings(""), list())

  checks <- check_rows()
  checks$settings[1] <- "buffer 3000"
  expect_error(qc_read_rules(write_rules(checks)), "not in the form name = value")
})
