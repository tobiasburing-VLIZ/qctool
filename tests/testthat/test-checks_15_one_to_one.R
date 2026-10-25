p01 <- function(code) paste0("http://vocab.nerc.ac.uk/collection/P01/current/", code, "/")
lsid <- function(id) paste0("urn:lsid:marinespecies.org:taxname:", id)

test_that("scientificname_id_one_to_one: one name with two IDs, one ID with two names, and empty sides", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = paste0("O", 1:8), occurrenceID = paste0("O", 1:8),
    scientificName = c("Abra alba", "Abra alba", "Abra alba", "Abra alba", "Abra alba Wood, 1802", "Abra alba", NA, "Mya arenaria"),
    scientificNameID = c(lsid(141433), lsid(141433), lsid(141433), lsid(""), lsid(141433), NA, lsid(141433), lsid(140430)))))
  issues <- run_check("scientificname_id_one_to_one", dataset)

  two_ids <- issues[grepl("several scientificNameIDs", message)]
  expect_equal(two_ids$line, 5L)                                   # the cut-off LSID, not the usual one
  expect_match(two_ids$suggestion, "141433", fixed = TRUE)
  expect_true(is.na(two_ids$severity_hint))                         # the check's own severity (error)

  two_names <- issues[grepl("several scientificNames", message)]
  expect_equal(two_names$line, 6L)                                  # with the author
  expect_equal(two_names$severity_hint, "warning")
  expect_match(two_names$suggestion, "Abra alba$")

  expect_equal(issues[message == "scientificNameID is empty, while other records with this scientificName have one", suggestion], lsid(141433))
  expect_equal(issues[message == "scientificName is empty, while other records with this scientificNameID have one", suggestion], "Abra alba")
  expect_false(any(grepl("Mya", issues$value)))                     # one name, one ID: fine
})

test_that("measurementType and measurementUnit go one-to-one with their IDs", {
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1"),
    emof = data.frame(id = "E1", occurrenceID = "O1",
                      measurementType = c("length", "length", "length", "body length"),
                      measurementTypeID = c(p01("OBSINDLX"), p01("OBSINDLX"), p01("TL01XX01"), p01("OBSINDLX")),
                      measurementValue = c("1", "2", "3", "4"),
                      measurementUnit = c("mm", "mm", "mm", "millimetre"),
                      measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/UXMM/")))
  types <- run_check("measurementtype_id_one_to_one", dataset)
  expect_equal(sort(types$line), c(4L, 5L))
  expect_equal(types[line == 4, severity_hint], NA_character_)      # 'length' with a second type ID
  expect_equal(types[line == 5, severity_hint], "warning")          # the type ID with a second text
  expect_equal(run_check("measurementunit_id_one_to_one", dataset)$line, 5L)
})

test_that("measurementvalueid_consistent: one value ID for several texts or for measured numbers", {
  s11 <- function(code) paste0("http://vocab.nerc.ac.uk/collection/S11/current/", code, "/")
  dataset <- qc_read_dwca(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1"),
    emof = data.frame(id = "E1", occurrenceID = "O1",
                      measurementType = c("life stage", "life stage", "life stage", "fatty acid", "fatty acid", "count"),
                      measurementValue = c("adult", "adult", "Adult", "0.12", "0.4", "3"),
                      measurementValueID = c(s11("S1116"), s11("S1116"), s11("S1116"),
                                             "http://vocab.nerc.ac.uk/collection/S01/current/S016/",
                                             "http://vocab.nerc.ac.uk/collection/S01/current/S016/", NA))))
  issues <- run_check("measurementvalueid_consistent", dataset)
  expect_equal(issues[grepl("several measurementValues", message), line], 4L)
  expect_equal(issues[grepl("different measured numbers", message), line], c(5L, 6L))
})

test_that("values_and_ids_one_to_one uses the pairs of the reference list", {
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = paste0("O", 1:4), occurrenceID = paste0("O", 1:4),
    institutionCode = c("VLIZ", "VLIZ", "VLIZ", "NIOZ"),
    institutionID = c("https://edmo.seadatanet.org/report/3330", "https://edmo.seadatanet.org/report/3330",
                      "https://edmo.seadatanet.org/report/422", "https://edmo.seadatanet.org/report/422"),
    organismID = c("A", "A", "B", "B"), scientificName = c("Abra alba", "Abra alba", "Abra alba", "Mya arenaria"))))
  issues <- run_check("values_and_ids_one_to_one", dataset)
  expect_true(all(c("institutionCode; institutionID", "scientificName; organismID") %in% issues$field))
  # 'one way': a taxon with several organisms is fine, an organism with two taxa is not
  organisms <- issues[field == "scientificName; organismID"]
  expect_equal(nrow(organisms), 1)
  expect_match(organisms$message, "same organismID goes with several scientificNames")
})

test_that("qc_one_to_one shows each check once, with the severity found", {
  result <- qc_check(make_archive(occurrence = data.frame(
    id = c("O1", "O2"), occurrenceID = c("O1", "O2"), scientificName = c("Abra alba", "Abra alba Wood"),
    scientificNameID = lsid(141433))), online = FALSE)
  short <- qc_one_to_one(result)
  expect_equal(short[check_id == "scientificname_id_one_to_one", severity], "warning")
  expect_true(short[check_id == "scientificname_id_one_to_one", records] == 1)
})
