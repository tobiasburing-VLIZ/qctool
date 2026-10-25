# A three-level hierarchy like hydrozoa: the date only on the cruise,
# coordinates on the stations, one sample without anything.
hierarchy_archive <- function() {
  make_archive(
    event = data.frame(
      id            = c("CRUISE", "ST1", "ST2", "S1", "S2", "S3"),
      eventID       = c("CRUISE", "ST1", "ST2", "S1", "S2", "S3"),
      parentEventID = c(NA, "CRUISE", "CRUISE", "ST1", "ST1", "ST2"),
      eventDate     = c("1991/1994", NA, NA, NA, "1992-05-03", NA),
      decimalLatitude = c(NA, "64.1", "65.2", NA, NA, NA)),
    occurrence = data.frame(
      id = c("S1", "S2", "S3"), occurrenceID = c("O1", "O2", "O3"),
      eventDate = c(NA, NA, "1993-07-01")))
}

test_that("links between tables are row numbers", {
  dataset <- qc_read_dwca(hierarchy_archive())
  expect_equal(event_parent_row(dataset), c(NA, 1L, 1L, 2L, 2L, 3L))
  expect_equal(occurrence_event_row(dataset), c(4L, 5L, 6L))
})

test_that("events inherit from the nearest parent that has a value", {
  dataset <- qc_read_dwca(hierarchy_archive())
  date <- inherited_value(dataset, "event", "eventDate")
  expect_equal(date$value, c("1991/1994", "1991/1994", "1991/1994", "1991/1994", "1992-05-03", "1991/1994"))
  expect_equal(date$from_row, c(1L, 1L, 1L, 1L, 5L, 1L))

  latitude <- inherited_value(dataset, "event", "decimalLatitude")
  expect_equal(latitude$value, c(NA, "64.1", "65.2", "64.1", "64.1", "65.2"))
})

test_that("occurrences inherit from their event; own values win", {
  dataset <- qc_read_dwca(hierarchy_archive())
  date <- inherited_value(dataset, "occurrence", "eventDate")
  expect_equal(date$value, c("1991/1994", "1992-05-03", "1993-07-01"))
  expect_equal(date$from_table, c("event", "event", "occurrence"))
  expect_equal(date$from_row, c(1L, 5L, 3L))
})

test_that("fields that do not inherit keep only own values", {
  dataset <- qc_read_dwca(hierarchy_archive())
  expect_true(all(is.na(inherited_value(dataset, "occurrence", "scientificName")$value)))
})

test_that("a loop in the hierarchy does not hang the tool", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = c("A", "B"), eventID = c("A", "B"), parentEventID = c("B", "A"), eventDate = c(NA, NA))))
  expect_equal(inherited_value(dataset, "event", "eventDate")$value, c(NA_character_, NA_character_))
})

test_that("leaf events and events with occurrences hold the records", {
  dataset <- qc_read_dwca(hierarchy_archive())
  expect_equal(events_that_hold_records(dataset), 4:6)
})

test_that("settings fall back to the default written in the check", {
  expect_equal(setting(list(buffer_m = 500), "buffer_m", 3000), 500)
  expect_equal(setting(list(), "buffer_m", 3000), 3000)
})
