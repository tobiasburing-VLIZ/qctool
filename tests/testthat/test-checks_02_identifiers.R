ids_archive <- function(event_ids = c("E1", "E2", "E3"), parents = c(NA, "E1", "E1"),
                        occurrence = NULL, emof = NULL) {
  tables <- list(event = data.frame(id = event_ids, eventID = event_ids, parentEventID = parents))
  if (!is.null(occurrence)) tables$occurrence <- occurrence
  if (!is.null(emof)) tables$emof <- emof
  qc_read_dwca(do.call(make_archive, tables))
}

test_that("eventid_unique finds every copy of a doubled eventID", {
  dataset <- ids_archive(event_ids = c("E1", "E2", "E2"), parents = c(NA, NA, NA))
  issues <- run_check("eventid_unique", dataset)
  expect_equal(issues$line, c(3L, 4L))
  expect_equal(nrow(run_check("eventid_unique", ids_archive())), 0)
})

test_that("parenteventid_exists finds parents that do not exist, not empty ones", {
  issues <- run_check("parenteventid_exists", ids_archive(parents = c(NA, "E1", "E9")))
  expect_equal(issues$value, "E9")
})

test_that("event_hierarchy_no_loops finds the events in a loop", {
  issues <- run_check("event_hierarchy_no_loops", ids_archive(parents = c("E3", "E1", "E2")))
  expect_equal(sort(issues$record_id), c("E1", "E2", "E3"))
  issues <- run_check("event_hierarchy_no_loops", ids_archive(parents = c(NA, "E2", "E1")))
  expect_equal(issues$record_id, "E2")    # its own parent
})

test_that("extension_links_to_core finds orphans", {
  dataset <- ids_archive(occurrence = data.frame(id = c("E1", "E9"), occurrenceID = c("O1", "O2")))
  issues <- run_check("extension_links_to_core", dataset)
  expect_equal(issues$record_id, "O2")
  expect_match(issues$message, "does not exist")
})

test_that("extension_eventid_matches_coreid finds a second, different event link", {
  dataset <- ids_archive(occurrence = data.frame(id = c("E2", "E3"), occurrenceID = c("O1", "O2"), eventID = c("E2", "E1")))
  issues <- run_check("extension_eventid_matches_coreid", dataset)
  expect_equal(issues$record_id, "O2")
  expect_match(issues$message, "names another event")
})

# --- Broken links, every combination (feedback 2026-10-25) ---------------------------

broken_event_core <- function() {
  qc_read_dwca(make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), parentEventID = c(NA, "E1")),
    occurrence = data.frame(id = c("E2", "E8", "E1"), eventID = c("E2", "E8", "E7"), occurrenceID = c("O1", "O2", "O3")),
    emof = data.frame(id = c("E2", "E2", "E9", "E1"), occurrenceID = c("O1", "O99", NA, "O1"), eventID = c("E2", "E2", "E9", "E1"),
                      measurementType = "x"),
    dna = data.frame(id = c("E2", "E2"), occurrenceID = c("O1", "O77"), DNA_sequence = "ACGT")))
}

test_that("event core: occurrences, measurements and DNA records pointing to missing events or occurrences", {
  dataset <- broken_event_core()
  orphans <- run_check("extension_links_to_core", dataset)
  expect_equal(sort(orphans$value), c("E8", "E9"))
  expect_match(orphans[table == "occurrence", message], "The occurrence points to an event that does not exist in the event table")
  expect_match(orphans[table == "emof", message], "The measurement points to an event")

  wrong_eventid <- run_check("extension_eventid_matches_coreid", dataset)
  expect_equal(wrong_eventid$value, "E7")                                       # O3: eventID E7, linked to E1
  expect_match(wrong_eventid$message, "names an event that does not exist")

  expect_equal(run_check("emof_occurrenceid_exists", dataset)$value, "O99")
  expect_equal(run_check("dna_occurrenceid_exists", dataset)$value, "O77")      # DNA was not checked before
  expect_match(run_check("emof_event_matches_occurrence", dataset)$suggestion, "E2")   # O1 is on E2, the measurement on E1
})

test_that("occurrence core: eMoF and DNA records pointing to missing or other occurrences, each reported once", {
  dataset <- qc_read_dwca(make_archive(
    occurrence = data.frame(id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3")),
    emof = data.frame(id = c("O1", "O9", "O2", "O2"), occurrenceID = c("O1", "O9", "O3", "O8"), measurementType = "x"),
    dna = data.frame(id = c("O1", "O7"), occurrenceID = c("O1", "O7"), DNA_sequence = "ACGT")))
  expect_equal(sort(run_check("extension_links_to_core", dataset)$value), c("O7", "O9"))
  mismatch <- run_check("extension_occurrenceid_matches_coreid", dataset)
  expect_equal(mismatch$value, c("O3", "O8"))
  expect_match(mismatch$message[1], "names another occurrence")      # O3 exists, but the link says O2
  expect_match(mismatch$message[2], "does not exist")                # O8 does not exist
  expect_error(check_emof_occurrenceid_exists(dataset, list()), class = "qc_not_applicable")   # no double reporting of O9
})

test_that("without an eventID column the id of the event core counts: broken parents are still found", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = c("E1", "E2", "E2"), parentEventID = c(NA, "X", "E1"))))
  expect_equal(run_check("parenteventid_exists", dataset)$value, "X")
  expect_equal(run_check("eventid_unique", dataset)$field, c("id (core link)", "id (core link)"))
  expect_equal(event_parent_row(dataset), c(NA, NA, 1L))
})

test_that("a missing date is blamed on the broken link, not only on the data", {
  result <- qc_check(make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), parentEventID = c(NA, "E9"), eventDate = c("2020-05-01", NA)),
    occurrence = data.frame(id = c("E1", "E8"), occurrenceID = c("O1", "O2"), scientificName = "Abra alba")),
    checks = "required_fields_filled", online = FALSE)
  messages <- result$issues_grouped[check_id == "required_fields_filled" & field == "eventDate", message]
  expect_true(any(grepl("the occurrence is not linked to an existing event", messages)))   # O2 points to E8
  expect_true(any(grepl("its parent event does not exist", messages)))                    # E2's parent E9
})

test_that("occurrenceid_unique and occurrenceid_globally_unique: running numbers are fine", {
  dataset <- ids_archive(occurrence = data.frame(id = c("E1", "E2", "E3", "E4"), occurrenceID = c("1", "1", "VLIZ:XY:3", "A1")))
  expect_equal(run_check("occurrenceid_unique", dataset)$line, c(2L, 3L))
  issues <- run_check("occurrenceid_globally_unique", dataset)
  expect_equal(issues$line, 5L)                         # only 'A1': running numbers are fine (unique within the dataset is enough)
  expect_equal(qc_read_rules()$checks[check_id == "occurrenceid_globally_unique", severity], "off")
})

test_that("eMoF links: occurrenceID exists, same event, linked at all", {
  dataset <- ids_archive(
    occurrence = data.frame(id = c("E2", "E3"), occurrenceID = c("O1", "O2")),
    emof = data.frame(id = c("E2", "E2", "E3"), occurrenceID = c("O1", "O2", "O9"), measurementType = "x"))
  expect_equal(run_check("emof_occurrenceid_exists", dataset)$value, "O9")
  wrong_event <- run_check("emof_event_matches_occurrence", dataset)
  expect_equal(wrong_event$line, 3L)
  expect_match(wrong_event$suggestion, "E3")
  expect_equal(nrow(run_check("emof_record_linked", dataset)), 0)
})

test_that("measurementid_unique finds doubled measurement IDs", {
  dataset <- ids_archive(emof = data.frame(id = c("E1", "E2"), measurementID = c("M1", "M1"), measurementType = "x"))
  expect_equal(nrow(run_check("measurementid_unique", dataset)), 2)
})

test_that("eventid_equals_occurrenceid spots occurrence data in an event core", {
  dataset <- ids_archive(parents = c(NA, NA, NA), occurrence = data.frame(id = c("E1", "E2", "E3"), occurrenceID = c("E1", "E2", "E3")))
  issues <- run_check("eventid_equals_occurrenceid", dataset)
  expect_equal(nrow(issues), 3)
  expect_equal(unique(issues$value), "3 of 3 occurrences")
})
