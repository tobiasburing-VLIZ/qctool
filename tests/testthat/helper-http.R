# =============================================================================
# Test helper: online lookups without the internet
#
#   calls <- use_recorded_http()     # in a test_that() block
#
# http_fetch() is replaced by a function that gives the answer recorded earlier
# for the same request (fixtures/http_recorded.rds), and the disk cache goes to
# a temporary folder. calls$n counts the requests the checks made.
#
# To record the answers (again), run the tests once online with
#   Sys.setenv(QCTOOL_RECORD = "1"); devtools::test(filter = "lookups")
# =============================================================================

use_recorded_http <- function(env = parent.frame()) {
  file <- testthat::test_path("fixtures", "http_recorded.rds")
  recorded <- if (file.exists(file)) readRDS(file) else list()
  record <- identical(Sys.getenv("QCTOOL_RECORD"), "1")
  real_fetch <- get("http_fetch", envir = asNamespace("qctool"))
  calls <- new.env()
  calls$n <- 0

  replay <- function(requests, service, tries = 3) {
    calls$n <- calls$n + length(requests)
    keys <- vapply(requests, request_key, character(1))
    if (record) {
      responses <- real_fetch(requests, service, tries)
      for (i in seq_along(keys)) recorded[[keys[i]]] <<- responses[[i]][c("status_code", "content")]
      dir.create(dirname(file), showWarnings = FALSE)
      saveRDS(recorded, file)
      return(responses)
    }
    lapply(keys, function(key) {
      if (is.null(recorded[[key]])) stop("No recorded answer for this request; record with QCTOOL_RECORD=1: ", substr(key, 1, 200))
      recorded[[key]]
    })
  }

  testthat::local_mocked_bindings(http_fetch = replay, internet_available = function() TRUE, .package = "qctool", .env = env)
  withr::local_options(qctool.online = TRUE, qctool.cache_dir = withr::local_tempdir(.local_envir = env), .local_envir = env)
  invisible(calls)
}

request_key <- function(request) paste(request$url, if (is.null(request$body)) "" else request$body)

# A service that does not answer.
use_failing_http <- function(env = parent.frame()) {
  failing <- function(requests, service, tries = 3) {
    stop(structure(class = c("qc_http_failed", "error", "condition"),
                   list(message = paste(service, "did not answer (timeout)"), call = NULL)))
  }
  testthat::local_mocked_bindings(http_fetch = failing, internet_available = function() TRUE, .package = "qctool", .env = env)
  withr::local_options(qctool.online = TRUE, qctool.cache_dir = withr::local_tempdir(.local_envir = env), .local_envir = env)
}

nerc <- function(collection, code) sprintf("http://vocab.nerc.ac.uk/collection/%s/current/%s/", collection, code)
lsid <- function(id) paste0("urn:lsid:marinespecies.org:taxname:", id)

# A dataset for the WoRMS, NERC and xylookup checks. Positions: E1 Ostend (at
# sea, 113 m from the beach), E2 Brussels (on land), E3 Mariana Trench.
online_test_archive <- function() {
  make_archive(
    event = data.frame(id = c("E1", "E2", "E3"), eventID = c("E1", "E2", "E3"), eventDate = "2020-05-01",
                       decimalLatitude = c("51.24", "50.85", "11.35"), decimalLongitude = c("2.92", "4.35", "142.2"),
                       maximumDepthInMeters = c("5", NA, "12000")),
    occurrence = data.frame(
      id = c("E1", "E1", "E1", "E1", "E1", "E1", "E2", "E3"),
      eventID = c("E1", "E1", "E1", "E1", "E1", "E1", "E2", "E3"),
      occurrenceID = paste0("O", 1:8),
      scientificName = c("Abra albaa", "Abra alba", "Nettastoma melanurum", "Gammarus pulex", "Lumbricus terrestris",
                         "Actinopterygii", "Abra alba", "Solea solea"),
      scientificNameID = lsid(c(141433, 999999999, 126308, 234074, 1069627, 10194, 141433, 141433)),
      occurrenceStatus = "present"),
    emof = data.frame(
      id = "E1", occurrenceID = c("O1", "O1", "O2", "O3", "O4", "O5"),
      measurementType = c("abundance", "Length", "Weigth", "Salinity", "Length", "unknown"),
      measurementTypeID = c(nerc("P01", "OCOUNT01"), nerc("P01", "OBSINDLX"), nerc("P01", "SPWGXX01"),
                            nerc("P01", "PPSUZZ01"), nerc("P01", "OBSINDLX"), nerc("P01", "XXXXXX99")),
      measurementValue = c("3", "12", "4", "35", "7", "1"),
      measurementUnit = c("individuals", "cm", "g", "psu", "ind L-1", NA),
      measurementUnitID = c(nerc("P06", "UUUU"), nerc("P06", "ULCM"), nerc("P06", "UGRM"),
                            nerc("P06", "UUUU"), nerc("P06", "UPMS"), nerc("P01", "OCOUNT01"))))
}
