p01 <- function(code) paste0("http://vocab.nerc.ac.uk/collection/P01/current/", code, "/")
p06 <- function(code) paste0("http://vocab.nerc.ac.uk/collection/P06/current/", code, "/")

emof_archive <- function(emof, occurrence_status = c("present", "absent"), occurrence_extra = list()) {
  occurrence <- data.frame(id = c("E1", "E1"), occurrenceID = c("O1", "O2"), occurrenceStatus = occurrence_status)
  for (name in names(occurrence_extra)) occurrence[[name]] <- occurrence_extra[[name]]
  qc_read_dwca(make_archive(event = data.frame(id = "E1", eventID = "E1"), occurrence = occurrence,
                            emof = cbind(data.frame(id = "E1"), emof)))
}

test_that("type ID and unit ID filled", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O1"), measurementType = c("abundance", "length"),
                                     measurementTypeID = c(p01("OCOUNT01"), NA), measurementValue = c("3", "12"),
                                     measurementUnit = c(NA, "mm"), measurementUnitID = c(NA, "NA")))
  expect_equal(run_check("measurementtypeid_filled", dataset)$value, "length")
  expect_equal(run_check("measurementunitid_filled", dataset)$value, "mm")
  expect_equal(run_check("value_has_id_or_unit", dataset)$value, "abundance")
})

test_that("measurementvalueid_consistent finds a missing value ID", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O2", "O2"), measurementType = "life stage",
                                     measurementValue = c("adult", "adult", "juvenile"),
                                     measurementValueID = c("http://vocab.nerc.ac.uk/collection/S11/current/S1116/", NA, NA)))
  issues <- run_check("measurementvalueid_consistent", dataset)
  expect_equal(issues$line, 3L)
})

test_that("nvs_uri_format repairs NERC URIs", {
  dataset <- emof_archive(data.frame(occurrenceID = "O1", measurementType = "count",
                                     measurementTypeID = "https://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01",
                                     measurementValue = "1"))
  issues <- run_check("nvs_uri_format", dataset)
  expect_equal(issues$suggestion, p01("OCOUNT01"))
})

test_that("unit_consistent_per_type and value_numeric_when_unit", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O1", "O2"), measurementType = "length",
                                     measurementValue = c("12", "13", "ca. 5"), measurementUnit = c("mm", "mm", "cm")))
  expect_equal(run_check("unit_consistent_per_type", dataset)$value, "cm")
  expect_equal(run_check("value_numeric_when_unit", dataset)$value, "ca. 5")
})

test_that("'not applicable' (P06 XXXX) is not a unit: text values are fine with it", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O2"), measurementType = "sex", measurementValue = c("female", "male"),
                                     measurementUnit = "Not applicable", measurementUnitID = p06("XXXX")))
  expect_equal(nrow(run_check("value_numeric_when_unit", dataset)), 0)
})

test_that("quantities that contradict occurrenceStatus", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O2"), measurementType = "count",
                                     measurementTypeID = p01("OCOUNT01"), measurementValue = c("0", "5")))
  expect_equal(run_check("zero_quantity_with_presence", dataset)$line, 2L)
  expect_equal(run_check("positive_quantity_with_absence", dataset)$line, 3L)
})

test_that("sampling effort and instrument", {
  without <- emof_archive(data.frame(occurrenceID = "O1", measurementType = "count", measurementTypeID = p01("OCOUNT01"), measurementValue = "1"))
  expect_equal(nrow(run_check("sampling_effort_present", without)), 1)
  expect_equal(nrow(run_check("sampling_instrument_present", without)), 1)
  with <- emof_archive(data.frame(occurrenceID = NA, measurementType = c("sampled area", "sampling instrument"),
                                  measurementTypeID = c(p01("AREABEDS"), p01("NMSPINST")), measurementValue = c("0.1", "van Veen grab")))
  expect_equal(nrow(run_check("sampling_effort_present", with)), 0)
  expect_equal(nrow(run_check("sampling_instrument_present", with)), 0)
})

test_that("individualcount_also_in_emof and organismquantity_use", {
  dataset <- emof_archive(data.frame(occurrenceID = c("O1", "O2"), measurementType = "count", measurementTypeID = p01("OCOUNT01"),
                                     measurementValue = c("5", "3")),
                          occurrence_status = c("present", "present"),
                          occurrence_extra = list(individualCount = c("5", "4"), organismQuantity = c("10", NA), organismQuantityType = c("% cover", NA)))
  expect_equal(run_check("individualcount_also_in_emof", dataset)$record_id, "O2")
  expect_match(run_check("organismquantity_use", dataset)$message, "eMoF")
})

test_that("emof_type_has_dwc_field", {
  dataset <- emof_archive(data.frame(occurrenceID = "O1", measurementType = "sampling protocol",
                                     measurementTypeID = p01("SAMPPROT"), measurementValue = "grab"))
  expect_equal(run_check("emof_type_has_dwc_field", dataset)$suggestion, "use the field samplingProtocol")
})
