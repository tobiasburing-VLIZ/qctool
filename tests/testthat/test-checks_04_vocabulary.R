vocabulary_archive <- function(...) {
  qc_read_dwca(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", ...)))
}

test_that("basisOfRecord: other spellings get the right term as suggestion", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3"),
    basisOfRecord = c("MATERIAL_SAMPLE", "HumanObservation", "Fish survey"))))
  issues <- run_check("basisofrecord_valid", dataset)
  expect_equal(issues$value, c("MATERIAL_SAMPLE", "Fish survey"))
  expect_equal(issues$suggestion, c("MaterialSample", NA))
})

test_that("occurrenceStatus accepts detected/notDetected and corrects capitals", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = paste0("O", 1:4), occurrenceID = paste0("O", 1:4),
    occurrenceStatus = c("present", "notDetected", "Present", "1"))))
  issues <- run_check("occurrencestatus_valid", dataset)
  expect_equal(issues$suggestion, c("present", "present"))
})

test_that("taxonRank 'super kingdom' gets 'superkingdom'", {
  expect_equal(run_check("taxonrank_valid", vocabulary_archive(taxonRank = "super kingdom"))$suggestion, "superkingdom")
})

test_that("dctype_valid sends kinds of events to eventType", {
  issues <- run_check("dctype_valid", qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1", type = "cruise"))))
  expect_equal(issues$suggestion, "move to eventType")
})

test_that("countryCode, geodeticDatum, establishment terms", {
  expect_equal(run_check("countrycode_valid", vocabulary_archive(countryCode = "be"))$suggestion, "BE")
  expect_equal(nrow(run_check("countrycode_valid", vocabulary_archive(countryCode = "BE"))), 0)
  expect_equal(nrow(run_check("geodeticdatum_valid", vocabulary_archive(geodeticDatum = "EPSG:4326"))), 0)
  expect_equal(run_check("geodeticdatum_valid", vocabulary_archive(geodeticDatum = "WGS 1984"))$suggestion, "EPSG:4326")
  expect_equal(run_check("establishment_terms_valid", vocabulary_archive(establishmentMeans = "Introduced"))$suggestion, "introduced")
})

test_that("license_kind recognises the three accepted licences", {
  expect_equal(license_kind(c("http://creativecommons.org/licenses/by/4.0/legalcode", "CC-BY-NC 4.0",
                              "http://creativecommons.org/publicdomain/zero/1.0/legalcode", "CC-BY-SA",
                              "Creative Commons Attribution Non Commercial (CC-BY) 4.0 License", "All rights reserved")),
               c("CC-BY", "CC-BY-NC", "CC0", "other", "CC-BY-NC", "other"))
  # Short names are enough; no URL needed (feedback 2026-10-24).
  expect_equal(license_kind(c("cc", "CC", "cc-by", "CC BY 4.0", "ccby", "cc0", "CC0 1.0", "cc-by-nc", "cc by-sa")),
               c("CC-BY", "CC-BY", "CC-BY", "CC-BY", "CC-BY", "CC0", "CC0", "CC-BY-NC", "other"))
  expect_equal(nrow(run_check("license_field_valid", vocabulary_archive(license = "CC-BY-SA 4.0"))), 1)
  expect_equal(nrow(run_check("license_field_valid", vocabulary_archive(license = "cc-by"))), 0)
})
