# Text that is not valid UTF-8 (files saved as Windows-1252, e.g. by Excel):
# repaired when read, reported by text_encoding_valid, and no check fails on it.

# Text with Windows-1252 bytes: 'Liège', 'Brügge', 'Krøyer'.
cp1252 <- function(...) {
  x <- vapply(list(...), function(bytes) rawToChar(as.raw(bytes)), character(1))
  Encoding(x) <- "UTF-8"   # marked as UTF-8 by the reader, but the bytes are not
  x
}
liege <- c(0x4c, 0x69, 0xe8, 0x67, 0x65)
brugge <- c(0x42, 0x72, 0xfc, 0x67, 0x67, 0x65)

# Write the lines of a file as bytes: '@' stands for the byte 0xE9 (é in Windows-1252).
write_cp1252 <- function(lines, path) {
  text <- charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n"))
  text[text == charToRaw("@")] <- as.raw(0xe9)
  writeBin(text, path)
  path
}

test_that("shorten() leaves text that is not valid UTF-8 as it is, however many values", {
  bad <- cp1252(liege, brugge)
  expect_false(any(validUTF8(bad)))
  expect_equal(shorten(bad), bad)
  expect_equal(shorten(c(bad, strrep("a", 120)))[3], paste0(strrep("a", 99), "…"))
})

test_that("repair_invalid_text reads invalid text as Windows-1252 and says where it was", {
  data <- data.table(place = c(cp1252(liege), "Oostende"), other = "x")
  repaired <- repair_invalid_text(data)
  expect_equal(repaired$data$place, c("Liège", "Oostende"))
  expect_equal(repaired$invalid, data.table(column = "place", position = 1L, row = 1L))
  expect_equal(nrow(repair_invalid_text(data.table(a = "fine"))$invalid), 0)
})

test_that("an archive with Windows-1252 bytes: repaired, reported, and no check fails", {
  folder <- make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), eventDate = c("2020-05-01", "2020-05-02"),
                       decimalLatitude = c("51.2", "51.3"), decimalLongitude = c("2.9", "3.0"), locality = c("Li@ge", "Br@gge")),
    occurrence = data.frame(id = c("E1", "E2"), occurrenceID = c("O1", "O2"), scientificName = c("Abra alba", "Abra alb@"),
                            occurrenceStatus = c("present", "pr@sent"), sex = c("male", "m@le")))
  for (file in list.files(folder, pattern = "\\.txt$", full.names = TRUE)) write_cp1252(readLines(file), file)
  dataset <- qc_read_dwca(folder)
  expect_true(all(validUTF8(dataset$tables$event$locality)))
  expect_equal(dataset$tables$event$locality, c("Liége", "Brégge"))     # '@' was written as the byte of 'é'
  expect_equal(nrow(dataset$invalid_text$occurrence), 3)
  expect_match(dataset$notes[1], "were not valid UTF-8")

  result <- qc_check(dataset, online = FALSE)
  expect_false("check error" %in% result$checks$status)
  encoding <- qc_issues(result, check = "text_encoding_valid")
  expect_setequal(paste(encoding$table, encoding$field), c("event locality", "event locality", "occurrence scientificName",
                                                            "occurrence occurrenceStatus", "occurrence sex"))
  expect_match(encoding$message[1], "Not valid UTF-8 in the file")
  expect_equal(qc_issues(result, check = "occurrencestatus_valid")$value, "présent")   # checked as read
})

test_that("a CSV saved as Windows-1252 (also in its header): repaired and reported under the mapped term", {
  path <- write_cp1252(c("Date,Latitude,Longitude,Species,Locality,Remark@",
                         "2020-05-01,51.2,2.9,Abra alba,Li@ge,a",
                         "2020-05-02,51.3,3.0,Abra alba,Br@gge,b"), tempfile(fileext = ".csv"))
  dataset <- qc_read_table(path)
  expect_true("Remarké" %in% dataset$mapping$column)
  expect_equal(dataset$invalid_text$occurrence$column, c("locality", "locality"))
  result <- qc_check(dataset, online = FALSE)
  expect_false("check error" %in% result$checks$status)
  expect_equal(nrow(qc_issues(result, check = "text_encoding_valid")), 2)
})
