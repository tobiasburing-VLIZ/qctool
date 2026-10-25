test_that("depth checks: numeric with hints, order, range", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = paste0("E", 1:4), eventID = paste0("E", 1:4),
    minimumDepthInMeters = c("10 m", "10-20", "30", "-5"), maximumDepthInMeters = c("12", "20", "25", "12000"))))
  numeric <- run_check("depth_numeric", dataset)
  expect_equal(numeric$suggestion, c("10", "minimumDepthInMeters 10, maximumDepthInMeters 20"))
  expect_equal(run_check("depth_min_le_max", dataset)$record_id, "E3")
  expect_equal(run_check("depth_in_range", dataset, list(max_m = 11000))$value, c("-5", "12000"))
})

test_that("elevation_valid", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"),
                                                          minimumElevationInMeters = c("5", "abc"), maximumElevationInMeters = c("3", "10"))))
  issues <- run_check("elevation_valid", dataset)
  expect_equal(issues$record_id, c("E2", "E1"))
})

test_that("scientificnameid_worms_format proposes the LSID", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = paste0("O", 1:3), occurrenceID = paste0("O", 1:3),
    scientificNameID = c("urn:lsid:marinespecies.org:taxname:141433", "141433",
                         "https://www.marinespecies.org/aphia.php?p=taxdetails&id=141433"))))
  issues <- run_check("scientificnameid_worms_format", dataset)
  expect_equal(issues$suggestion, rep("urn:lsid:marinespecies.org:taxname:141433", 2))
})

test_that("scientificname_clean finds qualifiers, authorship, underscores and non-taxa", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = paste0("O", 1:6), occurrenceID = paste0("O", 1:6),
    scientificName = c("Abra alba", "Abra sp.", "Alcidae Leach, 1820", "Candidatus_Nitrosopelagicus",
                       "uncultured bacterium", "Abra cf. alba"))))
  issues <- run_check("scientificname_clean", dataset)
  expect_equal(issues$record_id, c("O2", "O3", "O4", "O5", "O6"))
  expect_equal(issues$suggestion[c(1, 2, 3)], c("'Abra' (the rest goes to identificationQualifier)",
                                                "'Alcidae' (the rest goes to scientificNameAuthorship)",
                                                "Candidatus Nitrosopelagicus"))
})
