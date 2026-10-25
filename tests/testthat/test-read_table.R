test_that("column names are mapped: exact, capitals, synonym, similar, none", {
  mapping <- qc_suggest_mapping(c("eventDate", "DecimalLatitude", "Longitude", "scientificNam", "Mesh_size"))
  expect_equal(mapping$term, c("eventDate", "decimalLatitude", "decimalLongitude", "scientificName", NA))
  expect_equal(mapping$how, c("exact", "capitals", "synonym", "similar", "none"))
})

test_that("'similar' does not propose far-fetched or DNA terms", {
  mapping <- qc_suggest_mapping(c("Mean_Pred", "occurrenceRemarks1"))
  expect_equal(mapping$term, c(NA, "occurrenceRemarks"))
})

test_that("your own mapping wins, and NA leaves a column out", {
  mapping <- qc_suggest_mapping(c("Date", "Day", "Station"), mapping = c(Day = NA, Station = "locality"))
  expect_equal(mapping$term, c("eventDate", NA, "locality"))
  expect_equal(mapping$how, c("synonym", "your mapping", "your mapping"))
  expect_error(qc_suggest_mapping("Date", mapping = c(Datum = "eventDate")), "not in the table")
})

test_that("two columns for the same term: the more certain one is kept", {
  mapping <- qc_suggest_mapping(c("lat", "decimalLatitude"))
  expect_equal(mapping$term, c(NA, "decimalLatitude"))
  expect_match(mapping$note[1], "already goes to the column 'decimalLatitude'")
  # A repeated column name: the first keeps the term (before, both lost it).
  repeated <- qc_suggest_mapping(c("Species", "d13C", "Species"))
  expect_equal(repeated$term, c("scientificName", NA, NA))
})

test_that("geology terms are not proposed for look-alike names ('Group' is not dwc:group)", {
  mapping <- qc_suggest_mapping(c("Group", "NameLatin", "group"))
  expect_equal(mapping$term, c(NA, "scientificName", "group"))      # the exact term name is still used
  expect_match(mapping$note[1], "rock layers")
})

test_that("a semicolon CSV is read as a table with term names and line numbers", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("Date;Latitude;Longitude;species;Mesh_size", "2020-08-05;-51.669;-57.7466;Munida gregaria;350",
               "2020-08-06;-51.7;-57.8;Munida gregaria;350"), file)
  dataset <- qc_read_table(file)

  expect_equal(dataset$core, "occurrence")
  table <- dataset$tables$occurrence
  expect_true(all(c("eventDate", "decimalLatitude", "decimalLongitude", "scientificName", "column_5") %in% names(table)))
  expect_equal(table$.line, c(2L, 3L))
  expect_equal(dataset$files$delimiter, ";")
  expect_equal(dataset$mapping$term[5], NA_character_)
})

test_that("unmapped columns are reported, with a hint for numbers", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("eventDate,Mesh_size,Notes", "2020-08-05,350,calm sea", "2020-08-06,500,storm"), file)
  issues <- run_check("file_headers_valid", qc_read_table(file))
  expect_equal(issues$value, c("Mesh_size", "Notes"))
  expect_match(issues$suggestion[1], "eMoF")
  expect_no_match(issues$suggestion[2], "eMoF")
})

test_that("Excel dates become ISO 8601 and numbers keep the typed value", {
  file <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("data")
  wb$add_data("data", data.frame(eventDate = as.Date(c("2019-05-21", "2020-01-02")),
                                 decimalLatitude = c(43.55859, 43.7229970),
                                 occurrenceID = c("1", "2")))
  wb$save(file)

  dataset <- qc_read_table(file)
  table <- dataset$tables$occurrence
  expect_equal(table$eventDate, c("2019-05-21", "2020-01-02"))
  expect_equal(table$decimalLatitude, c("43.55859", "43.722997"))
  expect_match(dataset$notes, "2 Excel date cells")
  expect_match(dataset$files$file, "\\[data\\]")
})

test_that("qc_check takes a table directly", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("eventDate,locality", "2020-08-05,NA"), file)
  result <- qc_check(file)
  expect_equal(result$run$source_type, "table")
  expect_true("no_placeholder_values" %in% result$issues_grouped$check_id)
})
