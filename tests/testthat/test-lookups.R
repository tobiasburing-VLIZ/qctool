# Online lookups (WoRMS, NERC, OBIS xylookup). The answers of the services were
# recorded once (helper-http.R); these tests never use the internet.

test_that("WoRMS: unknown, unaccepted, not marine, above family, misspelled names", {
  use_recorded_http()
  dataset <- qc_read_dwca(online_test_archive())

  unknown <- run_check("scientificnameid_exists_in_worms", dataset)
  expect_equal(unknown$record_id, "O2")
  expect_match(unknown$suggestion, "taxname:141433 (Abra alba, exact match)", fixed = TRUE)

  accepted <- run_check("aphiaid_accepted", dataset)
  expect_equal(accepted$record_id, "O3")
  expect_match(accepted$suggestion, "taxname:299709", fixed = TRUE)

  expect_equal(run_check("taxon_is_marine", dataset)$record_id, c("O4", "O5"))
  expect_equal(run_check("taxon_identified_to_family_or_lower", dataset)$record_id, "O6")

  names <- run_check("scientificname_matches_aphiaid", dataset)
  expect_equal(names$record_id, c("O1", "O8"))          # 'Abra albaa' (1 letter), 'Solea solea' (another taxon)
  expect_equal(names$severity_hint, c("warning", NA))
  expect_equal(names$suggestion, c("Abra alba", "Abra alba"))
})

test_that("WoRMS: names without scientificNameID get a match as suggestion", {
  use_recorded_http()
  dataset <- qc_read_dwca(make_archive(occurrence = data.frame(
    id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3"), scientificName = c("Abra alba", "Abra albaa", "Xyzzy foo"))))
  issues <- run_check("names_without_id_matched", dataset)
  expect_equal(issues$record_id, c("O1", "O2", "O3"))
  expect_match(issues$suggestion[1], "taxname:141433 (Abra alba, exact match)", fixed = TRUE)
  expect_match(issues$suggestion[2], "Abra alba, phonetic match", fixed = TRUE)
  expect_match(issues$message[3], "not found in WoRMS")
})

test_that("OBIS xylookup: positions on land, marine taxa on land, depth below the sea floor", {
  use_recorded_http()
  dataset <- qc_read_dwca(online_test_archive())

  land <- run_check("points_on_land", dataset)
  expect_equal(land$record_id, "E2")                     # Brussels; Ostend beach (113 m at sea) is fine
  expect_match(land$value, "km inland")
  expect_equal(run_check("marine_taxon_on_land", dataset)$record_id, "O7")   # Abra alba in Brussels
  expect_equal(run_check("depth_vs_bathymetry", dataset)$record_id, "E3")    # 12000 m in the Mariana Trench
})

test_that("NERC: unknown and deprecated terms, units that do not fit the type or the text", {
  use_recorded_http()
  dataset <- qc_read_dwca(online_test_archive())

  expect_equal(run_check("measurementtypeid_valid", dataset)$value, nerc("P01", "XXXXXX99"))
  not_a_unit <- run_check("measurementunitid_valid", dataset)
  expect_equal(not_a_unit$value, nerc("P01", "OCOUNT01"))
  expect_match(not_a_unit$message, "not a unit")
  expect_equal(run_check("vocabulary_terms_not_deprecated", dataset)$suggestion, nerc("P01", "PSALZZXX"))

  units <- run_check("type_unit_compatible", dataset)
  expect_equal(units$line, c(3L, 6L))                     # cm for a length (same kind: info), per m2 for a length
  expect_equal(units$severity_hint, c("info", NA))

  text <- run_check("unit_text_matches_unitid", dataset)
  expect_equal(text$line, 6L)                             # 'ind L-1' with 'Number per square metre'

  types <- run_check("measurementtype_matches_typeid", dataset)
  expect_equal(types$suggestion[types$line == 4L], "weight")   # 'Weigth'
})

test_that("offline: the cache is used, otherwise the check is 'not performed'", {
  calls <- use_recorded_http()
  folder <- online_test_archive()

  offline <- qc_read_dwca(folder)
  offline$online <- FALSE
  expect_error(check_scientificnameid_exists_in_worms(offline, list()), class = "qc_not_performed")

  run_check("scientificnameid_exists_in_worms", qc_read_dwca(folder))   # online: fills the cache
  requests <- calls$n
  offline <- qc_read_dwca(folder)
  offline$online <- FALSE
  expect_equal(run_check("scientificnameid_exists_in_worms", offline)$record_id, "O2")
  expect_equal(calls$n, requests)                                         # nothing asked online
})

test_that("qc_check(online = FALSE) without a cache: not performed and skipped, with the reason", {
  use_recorded_http()
  result <- qc_check(online_test_archive(), online = FALSE,
                     checks = c("scientificnameid_exists_in_worms", "aphiaid_accepted", "measurementtypeid_valid"))
  status <- stats::setNames(result$checks$status, result$checks$check_id)
  expect_equal(unname(status[c("scientificnameid_exists_in_worms", "aphiaid_accepted", "measurementtypeid_valid")]),
               c("not performed", "skipped", "not performed"))
  expect_match(result$checks[check_id == "measurementtypeid_valid", reason], "offline")
  expect_false(result$run$online)
})

test_that("points_on_land offline uses the coarse coastline and says so", {
  skip_if_not_installed("maps")
  use_recorded_http()
  dataset <- qc_read_dwca(online_test_archive())
  dataset$online <- FALSE
  land <- run_check("points_on_land", dataset)
  expect_equal(land$record_id, "E2")
  expect_match(land$message, "approximate")
  expect_equal(land_offline(c(4.35, 2.0, NA), c(50.85, 52.0, 10), 3000), c(TRUE, FALSE, NA))
})

test_that("a service that does not answer: 'not performed' with the reason", {
  use_failing_http()
  result <- qc_check(online_test_archive(), checks = c("measurementtypeid_valid", "measurementunitid_valid"))
  expect_equal(result$checks[check_id %in% c("measurementtypeid_valid", "measurementunitid_valid"), unique(status)], "not performed")
  expect_match(result$checks[check_id == "measurementunitid_valid", reason], "did not answer")
})

test_that("the disk cache keeps answers with their date; old ones are fetched again", {
  withr::local_options(qctool.cache_dir = withr::local_tempdir(), qctool.cache_days = 30)
  local_mocked_bindings(internet_available = function() TRUE)
  cache_write("test_service", data.table(query = c("a", "b"), found = TRUE, fetched = Sys.Date() - c(1, 40)))
  expect_equal(cache_read("test_service")$query, c("a", "b"))

  fetched <- character()
  fetch <- function(batches) {
    keys <- unlist(batches, use.names = FALSE)
    fetched <<- c(fetched, keys)
    data.table(query = keys, found = TRUE)
  }
  dataset <- list(cache = new.env(), online = TRUE)
  result <- lookup_values(dataset, "test_service", c("a", "b", "c"), fetch, batch_size = 10, what = "things")
  expect_equal(sort(fetched), c("b", "c"))               # a from the cache, b too old, c new
  expect_equal(result$query, c("a", "b", "c"))
  expect_equal(nrow(cache_read("test_service")), 3)
  expect_equal(lookup_summary(dataset)$from_cache, 1)
  expect_length(qc_clear_cache("test_service"), 1)
})

test_that("answers that came are kept when other requests get no answer", {
  withr::local_options(qctool.online = TRUE, qctool.cache_dir = withr::local_tempdir())
  local_mocked_bindings(internet_available = function() TRUE, http_fetch = function(requests, service, tries = 3) {
    list(list(status_code = 200L, content = charToRaw("ok")), list(status_code = NA_integer_, error = "Timeout was reached"))
  })
  parse <- function(batch, response) data.table(query = batch, found = TRUE)
  fetch <- function(batches) fetch_and_parse(batches, list(list(url = "a"), list(url = "b")), "Test service", parse)
  dataset <- list(cache = new.env())

  expect_error(lookup_values(dataset, "test_service", c("a", "b"), fetch, batch_size = 1, what = "things"),
               "1 of 2 things could not be looked up: Test service did not answer (Timeout was reached)",
               fixed = TRUE, class = "qc_not_performed")
  expect_equal(cache_read("test_service")$query, "a")                                        # kept for the next run
  expect_true(lookup_values(dataset, "test_service", "a", fetch, batch_size = 1, what = "things")$found)
})

test_that("units written in different ways are recognised", {
  expect_equal(normalise_unit("ind/m2"), normalise_unit("Number per square metre"))
  expect_equal(normalise_unit("individuals m-2"), "/m^2")
  expect_equal(normalise_unit("Centimeters"), normalise_unit("Centimetres"))
  expect_false(normalise_unit("ind L-1") == normalise_unit("#/m^3"))
  expect_true(unit_text_matches("individuals", "Dimensionless", "Dmnless", "UUUU"))
  expect_true(unit_text_matches("mg/g", "Grams per kilogram", "g/kg", "UGKG"))
  expect_false(unit_text_matches("mm", "Centimetres", "cm", "ULCM"))
})

test_that("measurementType text is compared with the NERC label word by word", {
  expect_equal(compare_type_text("Weigth", "Wet weight of biological entity specified elsewhere", NA), "typo:weight")
  expect_equal(compare_type_text("LifeStage", "Development stage of biological entity specified elsewhere", NA), "same")
  expect_equal(compare_type_text("Gear", "Sampling instrument name", NA), "different")
})

test_that("depth_vs_bathymetry uses the minimum depth where no maximum is given", {
  local_mocked_bindings(xy_for_records = function(dataset, lon, lat) data.table(shoredistance = 1000, bathymetry = rep(50, length(lon))))
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = c("E1", "E2", "E3"), eventID = c("E1", "E2", "E3"), decimalLatitude = "51", decimalLongitude = "2",
    minimumDepthInMeters = c("400", "10", "400"), maximumDepthInMeters = c(NA, "20", "500"))))
  issues <- run_check("depth_vs_bathymetry", dataset)
  expect_equal(issues$record_id, c("E1", "E3"))
  expect_equal(issues$field, c("minimumDepthInMeters", "maximumDepthInMeters"))
})

test_that("name comparison ignores authorship and qualifiers; AphiaIDs from several forms", {
  expect_equal(name_distance(c("Abra alba (W. Wood, 1802)", "Abra cf. alba", "Abra albaa", "abra alba"), rep("Abra alba", 4)),
               c(0L, 0L, 1L, 1L))
  expect_equal(aphia_id(c(lsid(141433), "141433", NA, "NA")), c("141433", "141433", NA, NA))
})
