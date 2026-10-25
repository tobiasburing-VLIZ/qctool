test_that("a tab-separated archive with header rows is read with term names", {
  dataset <- qc_read_dwca(clean_archive())

  expect_s3_class(dataset, "qc_dataset")
  expect_equal(names(dataset$tables), c("event", "occurrence", "emof"))
  expect_equal(dataset$core, "event")
  expect_equal(dataset$info$title, "Test dataset")
  expect_true(all(c("eventID", "eventDate", ".coreid", ".line") %in% names(dataset$tables$event)))
  expect_equal(dataset$tables$event$.line, c(2L, 3L))          # line 1 is the header
  expect_equal(dataset$tables$occurrence$.coreid, c("E1", "E2"))
  expect_equal(nrow(dataset$problems), 0)
})

test_that("a comma-separated archive WITHOUT header row gets its names from meta.xml", {
  folder <- make_archive(
    event = data.frame(id = "E1", eventID = "E1", locality = "Blanes Bay, Mediterranean Sea"),
    header = FALSE, delimiter = ",", quote = TRUE)
  dataset <- qc_read_dwca(folder)

  expect_equal(dataset$tables$event$locality, "Blanes Bay, Mediterranean Sea")  # comma inside quotes kept
  expect_equal(dataset$tables$event$.line, 1L)                                  # no header line
  expect_equal(dataset$files$header_lines, 0L)
})

test_that("doubled quotes inside quoted values become single quotes (as in MGnify JSON)", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1", dynamicProperties = '{"depth":"10"}'),
                         delimiter = ",", quote = TRUE)
  expect_match(readLines(file.path(folder, "event.txt"))[2], '""depth""', fixed = TRUE)   # as written in the file
  expect_equal(qc_read_dwca(folder)$tables$event$dynamicProperties, '{"depth":"10"}')    # as read by the tool
})

test_that("values are kept as they are: spaces, 'NA' text, capitals", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1", datasetName = " My data ", locality = "NA"))
  dataset <- qc_read_dwca(folder)

  expect_equal(dataset$tables$event$datasetName, " My data ")
  expect_equal(dataset$tables$event$locality, "NA")
})

test_that("default values from meta.xml fill the column", {
  folder <- make_archive(
    occurrence = data.frame(id = c("O1", "O2"), occurrenceID = c("O1", "O2")),
    defaults = list(occurrence = c(basisOfRecord = "HumanObservation")))
  dataset <- qc_read_dwca(folder)

  expect_equal(dataset$tables$occurrence$basisOfRecord, c("HumanObservation", "HumanObservation"))
})

test_that("a file with only a header row becomes an empty table, not an error", {
  folder <- make_archive(event = data.frame(id = "E1", eventID = "E1"),
                         occurrence = data.frame(id = character(), occurrenceID = character()))
  dataset <- qc_read_dwca(folder)
  expect_equal(nrow(dataset$tables$occurrence), 0)
  expect_true(all(c("occurrenceID", ".coreid", ".line") %in% names(dataset$tables$occurrence)))
  expect_equal(nrow(dataset$problems), 0)
})

test_that("reading problems are collected instead of stopping", {
  no_meta <- tempfile("no_meta_")
  dir.create(no_meta)
  writeLines("a\tb", file.path(no_meta, "occurrence.txt"))
  dataset <- qc_read_dwca(no_meta)
  expect_match(dataset$problems$message, "meta.xml is missing")

  folder <- clean_archive()
  file.remove(file.path(folder, "emof.txt"))
  dataset <- qc_read_dwca(folder)
  expect_true(any(grepl("missing from the archive", dataset$problems$message)))
  expect_equal(names(dataset$tables), c("event", "occurrence"))
})

test_that("a zip file is read like a folder", {
  skip_if_not_installed("zip")
  folder <- clean_archive()
  zip_file <- tempfile(fileext = ".zip")
  zip::zipr(zip_file, files = list.files(folder, full.names = TRUE))

  dataset <- qc_read_dwca(zip_file)
  expect_equal(names(dataset$tables), c("event", "occurrence", "emof"))
  expect_false(is.na(dataset$source$checksum))
})
