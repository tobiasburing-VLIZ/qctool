# The verdict says when checks did not run (offline, a service down, a fault):
# a report must never look complete when parts of it are missing.

offline_unknown_taxon <- function() {
  qc_check(make_archive(occurrence = data.frame(
    id = "O1", occurrenceID = "O1", scientificName = "Abra alba", scientificNameID = "urn:lsid:marinespecies.org:taxname:999999999",
    occurrenceStatus = "present", basisOfRecord = "HumanObservation", eventDate = "2020-05-01",
    decimalLatitude = "51.2", decimalLongitude = "2.9")),
    online = FALSE, checks = c("scientificnameid_exists_in_worms", "eventdate_iso8601"))
}

test_that("blocking checks that did not run make the verdict 'not complete', with the areas named", {
  result <- offline_unknown_taxon()
  judged <- verdict(result)
  expect_equal(judged$state, "incomplete")
  expect_match(judged$title, "Not complete")
  expect_match(judged$text, "Taxonomy \\(WoRMS\\): 1")
  expect_equal(judged$not_run$check_id, "scientificnameid_exists_in_worms")
  expect_output(print(result), "NOT COMPLETE")
})

test_that("errors still come first, and checks that did not run are named with them", {
  result <- qc_check(make_archive(occurrence = data.frame(
    id = "O1", occurrenceID = "O1", scientificNameID = "urn:lsid:marinespecies.org:taxname:141433", eventDate = "21/05/2020")),
    online = FALSE, checks = c("scientificnameid_exists_in_worms", "eventdate_iso8601"))
  judged <- verdict(result)
  expect_equal(judged$state, "errors")
  expect_match(judged$text, "Did not run: 1 checks")
})

test_that("a complete run without problems is 'clean'", {
  result <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", eventDate = "2020-05-01")),
                     online = FALSE, checks = "eventdate_iso8601")
  expect_equal(verdict(result)$state, "clean")
  expect_equal(nrow(verdict(result)$not_run), 0)
})

test_that("an essential whose checks did not run is grey and says so", {
  essentials <- qc_essentials(offline_unknown_taxon())
  taxonomy <- essentials[essential == "Taxonomy"]
  expect_equal(taxonomy$light, "grey")
  expect_match(taxonomy$note, "not fully checked")
  expect_equal(essentials[essential == "Date", light], "green")
})

test_that("the overview shows the verdict and, right under it, the checks that did not run", {
  skip_if_not_installed("shiny")
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(offline_unknown_taxon())), {
    html <- output$top$html
    expect_match(html, "Not complete")
    expect_match(html, "did not run: their results are missing")
    expect_lt(regexpr("did not run: their results are missing", html), regexpr("Essentials", html))
  })
})

test_that("the Excel summary has the same verdict and lists the checks that did not run", {
  file <- tempfile(fileext = ".xlsx")
  qc_export_xlsx(offline_unknown_taxon(), file)
  summary <- openxlsx2::read_xlsx(file, sheet = "Summary", col_names = FALSE)
  text <- unlist(summary)
  expect_true(any(grepl("^NOT COMPLETE", text)))
  expect_true("scientificnameid_exists_in_worms" %in% text)
})
