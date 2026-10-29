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
  expect_true(is.na(two_names$severity_hint))                       # both ways an error
  expect_equal(two_names$field, "scientificNameID; scientificName")  # the field of the value with two partners first
  expect_equal(two_names$value, lsid(141433))                       # the ID with two names
  expect_equal(two_names$suggestion, "use one scientificName: Abra alba (3 records) or Abra alba Wood, 1802 (1 record)")

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
  expect_equal(types[line == 5, severity_hint], NA_character_)      # the type ID with a second text: also an error
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
  expect_true(all(c("institutionCode; institutionID", "organismID; scientificName") %in% issues$field))
  # 'one way': a taxon with several organisms is fine, an organism with two taxa is not
  organisms <- issues[field == "organismID; scientificName"]
  expect_equal(nrow(organisms), 1)
  expect_match(organisms$message, "same organismID goes with several scientificNames")
})

test_that("qc_one_to_one: every combination of a broken relationship, next to each other", {
  # 'metres' with unit ID 123A in 40 records and 123B in 3; 123A also with 'm' (3 records)
  n <- 46
  result <- qc_check(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = paste0("O", 1:n)),
    emof = data.frame(id = "E1", occurrenceID = paste0("O", 1:n), measurementType = "depth", measurementValue = as.character(1:n),
                      measurementUnit = c(rep("metres", 43), rep("m", 3)),
                      measurementUnitID = c(rep("123A", 40), rep("123B", 3), rep("123A", 3)))),
    checks = c("measurementunit_id_one_to_one", "scientificname_id_one_to_one"), online = FALSE)
  combos <- qc_one_to_one(result)
  expect_equal(combos$conflict, rep(c("measurementUnit 'metres' has 2 different measurementUnitIDs",
                                      "measurementUnitID '123A' has 2 different measurementUnits"), each = 2))
  expect_equal(paste(combos$value, "->", combos$other_value), c("metres -> 123A", "metres -> 123B", "123A -> metres", "123A -> m"))
  expect_equal(combos$records, c(40L, 3L, 40L, 3L))
  expect_equal(combos$less_common, c(FALSE, TRUE, FALSE, TRUE))
  expect_true(all(combos$severity == "error"))
  expect_equal(one_to_one_compared(result), "eMoF (measurements): measurementUnit and measurementUnitID")

  # The problem 'metres' on screen 3 shows the combinations of 'metres' only; its records are those with 123B.
  metres <- result$issues_grouped[value == "metres"]
  expect_equal(problem_combinations(result, metres)$other_value, c("123A", "123B"))
  expect_equal(qc_issues(result, problem = metres$problem)$line, 42:44)

  # Nothing broken: no combinations, but the pairs compared are known.
  fine <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba",
                                                        scientificNameID = lsid(141433))), online = FALSE)
  expect_equal(nrow(qc_one_to_one(fine)), 0)
  expect_equal(one_to_one_compared(fine), "Occurrence: scientificName and scientificNameID")
})

test_that("qc_one_to_one: values and valueIDs per measurement type, numbers with one valueID in one row", {
  s01 <- "http://vocab.nerc.ac.uk/collection/S01/current/S016/"
  result <- qc_check(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = "E1", occurrenceID = "O1"),
    emof = data.frame(id = "E1", occurrenceID = "O1", measurementType = c("life stage", "life stage", "fatty acid", "fatty acid"),
                      measurementValue = c("adult", "Adult", "0.12", "0.4"),
                      measurementValueID = c("S1116", "S1116", s01, s01))),
    checks = "measurementvalueid_consistent", online = FALSE)
  combos <- qc_one_to_one(result)
  expect_equal(combos[value == "S1116", conflict][1], "within measurementType 'life stage': measurementValueID 'S1116' has 2 different measurementValues")
  expect_equal(combos[value == s01, other_value], "2 different numbers (0.12, 0.4, ...)")
  numbers <- result$issues_grouped[grepl("measured numbers", message)]
  expect_equal(nrow(problem_combinations(result, numbers)), 1)
})

test_that("partner_list: most used first, 'record' for one, at most five", {
  expect_equal(partner_list("A", 2), "A (2 records)")
  expect_equal(partner_list(c("A", "B"), c(1200, 1)), "A (1,200 records) or B (1 record)")
  expect_equal(partner_list(LETTERS[1:7], 7:1), "A (7 records), B (6 records), C (5 records), D (4 records), E (3 records) and 2 more")
})
