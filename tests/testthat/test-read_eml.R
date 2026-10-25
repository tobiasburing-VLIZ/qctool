
test_that("the EML is read into plain parts", {
  meta <- qc_read_eml(xml2::read_xml(example_eml))

  expect_equal(meta$title, "Benthos of the Belgian part of the North Sea 2010-2020")
  expect_equal(meta$abstract, "First paragraph.\n\nSecond paragraph.")
  expect_equal(meta$package_id, "abc-123/v1.2")
  expect_equal(meta$ipt_url, "https://ipt.vliz.be/eurobis/resource?r=test_data")
  expect_equal(meta$doi, "https://doi.org/10.15468/abcd12")
  expect_equal(meta$pub_date, "2024-03-01")
  expect_equal(meta$license$url, "http://creativecommons.org/licenses/by/4.0/legalcode")
  expect_equal(meta$license$name, "Creative Commons Attribution (CC-BY) 4.0 License")
  expect_equal(meta$citation, "Example A (2024) Benthos of the BPNS. VLIZ.")
  expect_equal(meta$bibliography, "Example A (2021) A paper.")
})

test_that("people keep their role, e-mail and ORCID", {
  people <- qc_read_eml(xml2::read_xml(example_eml))$people
  expect_equal(people$role, c("creator", "contact", "metadataProvider"))
  expect_equal(people$orcid[1], "0000-0002-1825-0097")
  expect_equal(people$email, c("ann@example.org", "contact@example.org", "data@vliz.be"))
  expect_true(is.na(people$given_name[3]))
})

test_that("coverages, keywords, methods and project are read", {
  meta <- qc_read_eml(xml2::read_xml(example_eml))
  expect_equal(unlist(meta$geographic[, .(west, east, north, south)]), c(west = 2.2, east = 3.4, north = 51.9, south = 51.1))
  expect_equal(meta$temporal$begin, "2010-01-01")
  expect_equal(meta$temporal$end, "2020-12-31")
  expect_equal(meta$taxonomic$taxa$name, "Bivalvia")
  expect_equal(meta$keywords$keyword, c("Occurrence", "Observation"))
  expect_equal(meta$methods$steps, "Van Veen grab, 0.1 m2.")
  expect_equal(meta$methods$sampling, "Yearly in autumn.")
  expect_equal(meta$project$title, "LifeWatch")
  expect_equal(meta$maintenance$frequency, "annually")
})

test_that("a minimal EML gives NA and empty tables, not errors", {
  meta <- qc_read_eml(xml2::read_xml("<eml><dataset><title>Only a title</title></dataset></eml>"))
  expect_equal(meta$title, "Only a title")
  expect_true(is.na(meta$abstract))
  expect_equal(nrow(meta$people), 0)
  expect_equal(nrow(meta$geographic), 0)
  expect_true(is.na(meta$license$url))
})

test_that("the archive reader fills dataset$metadata", {
  dataset <- qc_read_dwca(clean_archive())
  expect_equal(dataset$metadata$title, "Test dataset")
  expect_equal(dataset$info$title, "Test dataset")
})
