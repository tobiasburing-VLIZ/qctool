# One block per check: a clean case that passes and a case that must be found.

clean <- function() qc_read_dwca(clean_archive())

test_that("all structure checks pass on a clean archive", {
  structure_checks <- c("archive_readable", "core_type_supported", "occurrence_table_present", "table_not_empty",
                        "emof_extension_used", "field_names_known", "field_in_expected_table", "field_datatype_valid",
                        "field_length_within_eurobis_limit", "file_headers_valid", "text_encoding_valid",
                        "no_line_breaks_in_values", "no_leading_trailing_spaces", "no_placeholder_values",
                        "no_empty_columns", "exact_duplicate_rows")
  dataset <- clean()
  for (check_id in structure_checks) {
    expect_equal(nrow(run_check(check_id, dataset)), 0, label = check_id)
  }
})

test_that("archive_readable reports reading problems", {
  folder <- clean_archive()
  file.remove(file.path(folder, "emof.txt"))
  issues <- run_check("archive_readable", qc_read_dwca(folder))
  expect_equal(issues$file, "emof.txt")
})

test_that("core_type_supported finds a Taxon core", {
  dataset <- qc_read_dwca(make_archive(taxon = data.frame(id = "T1", scientificName = "Abra alba")))
  expect_equal(run_check("core_type_supported", dataset)$field, "rowType")
})

test_that("occurrence_table_present finds an archive without occurrences", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1")))
  expect_equal(nrow(run_check("occurrence_table_present", dataset)), 1)
})

test_that("table_not_empty finds an empty extension", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = character(), occurrenceID = character())))
  expect_equal(run_check("table_not_empty", dataset)$field, "occurrence")
})

test_that("emof_extension_used finds the old MeasurementOrFact", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"),
                                       mof = data.frame(id = "E1", measurementType = "depth")))
  expect_equal(nrow(run_check("emof_extension_used", dataset)), 1)
})

test_that("field_names_known finds unknown terms and suggests the right one", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", rank = "species")))
  issues <- run_check("field_names_known", dataset)
  expect_equal(issues$field, "rank")
  expect_equal(issues$suggestion, "taxonRank")
})

test_that("field_names_known accepts both Dublin Core namespaces", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1", type = "Event"))
  meta <- readLines(file.path(folder, "meta.xml"))
  writeLines(sub("http://purl.org/dc/elements/1.1/type", "http://purl.org/dc/terms/type", meta, fixed = TRUE),
             file.path(folder, "meta.xml"))
  expect_equal(nrow(run_check("field_names_known", qc_read_dwca(folder))), 0)
})

test_that("field_in_expected_table finds event information in the occurrence extension", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1", samplingProtocol = "grab")))
  issues <- run_check("field_in_expected_table", dataset)
  expect_equal(issues$field, "samplingProtocol")
  expect_match(issues$message, "belongs in the event table")
})

test_that("field_datatype_valid finds a decimal comma and suggests a dot", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                                          sampleSizeValue = c("1,5", "2"))))
  issues <- run_check("field_datatype_valid", dataset)
  expect_equal(issues$value, "1,5")
  expect_equal(issues$suggestion, "1.5")
  expect_equal(issues$record_id, "E1")
})

test_that("field_datatype_valid leaves placeholders and dedicated fields to their own checks", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1", sampleSizeValue = "NA",
                                                          decimalLatitude = "52,1")))
  expect_equal(nrow(run_check("field_datatype_valid", dataset)), 0)
})

test_that("field_length_within_eurobis_limit: warning above the harvester limit, error above the database", {
  long_350 <- strrep("x", 400)
  long_1000 <- strrep("y", 1100)
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3"),
    associatedOccurrences = c(long_350, long_1000, "short"))))
  issues <- run_check("field_length_within_eurobis_limit", dataset)
  expect_equal(issues$record_id, c("O1", "O2"))
  expect_equal(issues$severity_hint, c("warning", NA))
})

test_that("field_length_within_eurobis_limit takes its limits from the rules workbook", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", institutionCode = strrep("I", 55))))
  dataset$rules <- qc_read_rules()
  expect_match(run_check("field_length_within_eurobis_limit", dataset)$message, "\\(50 characters\\)")
  dataset$rules$field_lengths[term == "institutionCode", max_length := 60L]    # as if changed in the sheet
  expect_equal(nrow(run_check("field_length_within_eurobis_limit", dataset)), 0)
})

test_that("file_headers_valid finds missing headers, wrong names and unmapped columns", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"), header = FALSE))
  expect_match(run_check("file_headers_valid", dataset)$message, "no header row")

  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1", decimalLatitude = "51", Lat = "51"))
  meta <- readLines(file.path(folder, "meta.xml"))
  writeLines(meta[!grepl("dwc/terms/Lat\"", meta)], file.path(folder, "meta.xml"))   # 'Lat' not mapped
  lines <- readLines(file.path(folder, "event.txt"))
  lines[1] <- sub("decimalLatitude", "decimallatitude", lines[1])
  writeLines(lines, file.path(folder, "event.txt"))

  issues <- run_check("file_headers_valid", qc_read_dwca(folder))
  expect_equal(issues$value, c("decimallatitude", "Lat"))
  expect_match(issues$message[1], "only in capitals")
  expect_match(issues$message[2], "not mapped")
})

test_that("text_encoding_valid finds broken characters and HTML codes, and suggests a repair", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = c("E1", "E2", "E3", "E4"), eventID = c("E1", "E2", "E3", "E4"),
    locality = c("Mesh 5 �m", "CafÃ©", "&quot;North&quot; bank", "Café"))))
  issues <- run_check("text_encoding_valid", dataset)
  expect_equal(issues$record_id, c("E1", "E2", "E3"))
  expect_equal(issues$suggestion[2:3], c("Café", "\"North\" bank"))
})

test_that("no_line_breaks_in_values finds tabs and line breaks", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                                          eventRemarks = c("two\nlines", "fine")),
                                       delimiter = ",", quote = TRUE))
  issues <- run_check("no_line_breaks_in_values", dataset)
  expect_equal(issues$record_id, "E1")
  expect_equal(issues$suggestion, "two lines")
})

test_that("no_leading_trailing_spaces finds spaces and no-break spaces", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3"),
    scientificName = c("Abra alba ", " Abra alba", "Abra alba"))))
  issues <- run_check("no_leading_trailing_spaces", dataset)
  expect_equal(issues$record_id, c("O1", "O2"))
  expect_equal(issues$suggestion, c("Abra alba", "Abra alba"))
})

test_that("no_placeholder_values finds NA, NULL and '-'", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = c("E1", "E2", "E3", "E4"), eventID = c("E1", "E2", "E3", "E4"),
    locality = c("NA", "null", "-", "Ostend"))))
  expect_equal(run_check("no_placeholder_values", dataset)$record_id, c("E1", "E2", "E3"))
})

test_that("no_empty_columns finds a column without any value", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                                          locality = c(NA, NA))))
  expect_equal(run_check("no_empty_columns", dataset)$field, "locality")
})

test_that("exact_duplicate_rows reports the copy and the line of the first", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    emof = data.frame(id = c("E1", "E1", "E1"), measurementType = c("depth", "depth", "temperature"),
                      measurementValue = c("10", "10", "12"))))
  issues <- run_check("exact_duplicate_rows", dataset)
  expect_equal(issues$line, 3L)
  expect_equal(issues$value, "same as line 2")
})

test_that("field_datatype_valid leaves a licence short name to license_field_valid", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2"), occurrenceID = c("O1", "O2"), license = c("CC-BY", "http://creativecommons.org/licenses/by/4.0/legalcode"))))
  expect_equal(nrow(run_check("field_datatype_valid", dataset)), 0)
  expect_equal(nrow(run_check("license_field_valid", dataset)), 0)
})
