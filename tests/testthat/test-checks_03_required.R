# The hydrozoa situation: date and institutionCode only on the cruise event.
inherited_archive <- function(sample_date = NA, occurrence_status = c("present", "present")) {
  make_archive(
    event = data.frame(id = c("CRUISE", "S1", "S2"), eventID = c("CRUISE", "S1", "S2"),
                       parentEventID = c(NA, "CRUISE", "CRUISE"),
                       eventDate = c("1991/1994", sample_date, NA),
                       decimalLatitude = c(NA, "64.1", "64.2"), decimalLongitude = c(NA, "-20.1", "-20.2"),
                       datasetName = c("Hydrozoa of Iceland", NA, NA), institutionCode = c("IMNH", NA, NA)),
    occurrence = data.frame(id = c("S1", "S2"), occurrenceID = c("O1", "O2"), scientificName = c("Obelia", "Obelia"),
                            scientificNameID = c("urn:lsid:marinespecies.org:taxname:117081", NA),
                            occurrenceStatus = occurrence_status, basisOfRecord = "PreservedSpecimen"),
    eml_title = "Hydrozoa of Iceland")
}

test_that("required values inherited from a parent event count as present", {
  issues <- run_check("required_fields_filled", qc_read_dwca(inherited_archive()))
  expect_false(any(issues$field %in% c("eventDate", "institutionCode", "datasetName")))
  expect_equal(issues$field, "scientificNameID")
  expect_equal(issues$record_id, "O2")
})

test_that("a missing value on an event is reported once, on the event", {
  folder <- inherited_archive()
  lines <- readLines(file.path(folder, "event.txt"))
  lines[2] <- sub("1991/1994", "", lines[2])          # remove the date from the cruise
  writeLines(lines, file.path(folder, "event.txt"))
  issues <- run_check("required_fields_filled", qc_read_dwca(folder))
  dates <- issues[field == "eventDate"]
  expect_equal(dates$table, c("event", "event"))
  expect_equal(dates$record_id, c("S1", "S2"))
})

test_that("a placeholder counts as missing, with its own message", {
  folder <- make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "NA",
                                                 basisOfRecord = "HumanObservation"))
  issues <- run_check("required_fields_filled", qc_read_dwca(folder))
  expect_match(issues[field == "scientificName", message], "only a placeholder")
})

test_that("only fields set to 'required' are treated as required", {
  required <- requirement_rows(list(rules = NULL), "required")
  expect_true(all(required$requirement == "required"))
  expect_false("coordinateUncertaintyInMeters" %in% required$term)   # recommended
  expect_false("catalogNumber" %in% required$term)                   # optional
})

test_that("required_fields_present reports missing columns", {
  folder <- make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1"))
  issues <- run_check("required_fields_present", qc_read_dwca(folder))
  expect_true(all(c("scientificName", "eventDate", "decimalLatitude", "basisOfRecord") %in% issues$field))
  expect_true(all(is.na(issues$line)))
})

test_that("datasetname_single_value and datasetname_equals_title", {
  folder <- make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                            datasetName = c("Benthos BPNS", "Benthos  BPNS")),
                         eml_title = "Benthos BPNS")
  dataset <- qc_read_dwca(folder)
  expect_equal(nrow(run_check("datasetname_single_value", dataset)), 2)
  differs <- run_check("datasetname_equals_title", dataset)
  expect_equal(differs$record_id, "E2")
  expect_match(differs$message, "similarity 92%")
  expect_equal(differs$suggestion, "Benthos BPNS")
})

test_that("missing_values_found_elsewhere finds 'Jan2008' in eventRemarks", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1", eventDate = NA, eventRemarks = "Jan2008"),
                         occurrence = data.frame(id = "E1", occurrenceID = "O1"))
  issues <- run_check("missing_values_found_elsewhere", qc_read_dwca(folder))
  expect_equal(issues[field == "eventDate", value], "eventRemarks: 'Jan2008'")
})

test_that("occurrencestatus_has_presence: only absences is reported", {
  expect_equal(nrow(run_check("occurrencestatus_has_presence", qc_read_dwca(inherited_archive(occurrence_status = c("absent", "absent"))))), 1)
  expect_equal(nrow(run_check("occurrencestatus_has_presence", qc_read_dwca(inherited_archive()))), 0)
})

test_that("modified_filled: no column is one issue, empty values one issue each", {
  without <- qc_read_dwca(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1")))
  issues <- run_check("modified_filled", without)
  expect_equal(nrow(issues), 1)
  expect_match(issues$message, "No modified column")
  with_empty <- qc_read_dwca(make_archive(occurrence = data.frame(id = c("O1", "O2"), occurrenceID = c("O1", "O2"),
                                                                  modified = c("2024-03-01", NA))))
  expect_equal(run_check("modified_filled", with_empty)$record_id, "O2")
  expect_equal(qc_read_rules()$checks[check_id == "modified_filled", severity], "info")
})
