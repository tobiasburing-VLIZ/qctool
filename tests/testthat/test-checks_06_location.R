positions_archive <- function(lat, lon, eml_box = NULL, ...) {
  folder <- make_archive(event = data.frame(id = paste0("E", seq_along(lat)), eventID = paste0("E", seq_along(lat)),
                                            decimalLatitude = lat, decimalLongitude = lon, ...))
  if (!is.null(eml_box)) {
    writeLines(sprintf(paste0('<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset><title>T</title><coverage><geographicCoverage>',
                              '<boundingCoordinates><westBoundingCoordinate>%s</westBoundingCoordinate><eastBoundingCoordinate>%s</eastBoundingCoordinate>',
                              '<northBoundingCoordinate>%s</northBoundingCoordinate><southBoundingCoordinate>%s</southBoundingCoordinate>',
                              '</boundingCoordinates></geographicCoverage></coverage></dataset></eml:eml>'),
                       eml_box[1], eml_box[2], eml_box[3], eml_box[4]), file.path(folder, "eml.xml"))
  }
  qc_read_dwca(folder)
}

test_that("suggest_decimal_degrees reads commas, degrees and hemispheres", {
  expect_equal(suggest_decimal_degrees(c("52,1", "51°13'30\"N", "51 13.5 S", "3.2W", "abc")),
               c("52.1", "51.225", "-51.225", "-3.2", NA))
})

test_that("coordinates_numeric, in range, 0-360, not zero", {
  dataset <- positions_archive(lat = c("51.2", "52,1", "95", "0", "40"), lon = c("2.9", "3.0", "3.0", "0", "200"))
  numeric <- run_check("coordinates_numeric", dataset)
  expect_equal(numeric$value, "52,1")
  expect_equal(numeric$suggestion, "52.1")
  expect_equal(run_check("coordinates_in_range", dataset)$value, "95")
  expect_equal(run_check("longitude_0_360", dataset)$suggestion, "-160")
  expect_equal(run_check("coordinates_not_zero", dataset)$record_id, "E4")
})

test_that("coordinates_swapped: impossible latitude, or swapped point inside the metadata box", {
  dataset <- positions_archive(lat = c("120", "2.9", "51.2"), lon = c("45", "51.2", "2.9"), eml_box = c(2, 4, 52, 51))
  issues <- run_check("coordinates_swapped", dataset)
  expect_equal(issues$record_id, c("E1", "E2"))
})

test_that("coordinates_precision finds Excel artefacts", {
  issues <- run_check("coordinates_precision", positions_archive(lat = c("43.558590000000002", "43.5"), lon = c("-7.1", "-7.2")))
  expect_equal(issues$suggestion, "43.55859")
})

test_that("coordinateuncertainty_valid", {
  issues <- run_check("coordinateuncertainty_valid", positions_archive(lat = c("51", "51", "51"), lon = c("3", "3", "3"),
                                                                       coordinateUncertaintyInMeters = c("100 m", "0", "500")))
  expect_equal(issues$value, c("100 m", "0"))
  expect_equal(issues$suggestion[1], "100")
})

test_that("coordinates_within_eml_area, also across the 180 degree meridian", {
  issues <- run_check("coordinates_within_eml_area", positions_archive(lat = c("51.5", "10"), lon = c("3", "3"), eml_box = c(2, 4, 52, 51)))
  expect_equal(issues$record_id, "E2")
  pacific <- positions_archive(lat = c("-17", "-17"), lon = c("179.5", "-179.5"), eml_box = c(179, -179, -16, -18))
  expect_equal(nrow(run_check("coordinates_within_eml_area", pacific)), 0)
})
