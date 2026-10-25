test_that("IPT addresses are turned into archive addresses", {
  expect_equal(archive_url_for("https://ipt.vliz.be/eurobis/resource?r=fifd_biodiversity_data"),
               "https://ipt.vliz.be/eurobis/archive.do?r=fifd_biodiversity_data")
  expect_equal(archive_url_for("https://ipt.vliz.be/upload/resource.do?r=turtles&v=1.7"),
               "https://ipt.vliz.be/upload/archive.do?r=turtles&v=1.7")
  expect_equal(archive_url_for("https://ipt.vliz.be/eurobis/archive.do?r=x"), "https://ipt.vliz.be/eurobis/archive.do?r=x")
  expect_equal(archive_url_for("https://example.org/data/dwca.zip"), "https://example.org/data/dwca.zip")
})

test_that("archive names come from the IPT short name or the file name", {
  expect_equal(archive_name("https://ipt.vliz.be/eurobis/archive.do?r=fifd_biodiversity_data"), "fifd_biodiversity_data.zip")
  expect_equal(archive_name("https://example.org/data/dwca.zip?x=1"), "dwca.zip")
})

test_that("OBIS and GBIF dataset pages are resolved through their API", {
  skip_if_offline()
  expect_equal(archive_url_for("https://obis.org/dataset/bbc947ad-48c0-4235-8398-3c92f0fc994a"),
               "https://nzobisipt.niwa.co.nz/archive.do?r=pacman_suva_edna")
  expect_match(archive_url_for("https://www.gbif.org/dataset/4fa7b334-ce0d-4e88-aaae-2e0c138d049e"), "\\.zip$")
})

test_that("a too large archive is refused (also when the server does not say its size)", {
  skip_if_offline()
  expect_error(qc_read_url("https://ipt.vliz.be/upload/resource?r=satellitetracking-turtles-mediterranean", max_size_mb = 0.01),
               "max_size_mb = 0.01")
})

test_that("what was downloaded is told by its content: archive, Excel, table or a web page", {
  archive_zip <- tempfile(fileext = ".zip")
  zip::zipr(archive_zip, list.files(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1")), full.names = TRUE))
  expect_equal(downloaded_kind(archive_zip, "https://example.org/download?id=1"), "archive")

  excel <- tempfile(fileext = ".xlsx")
  openxlsx2::write_xlsx(data.frame(occurrenceID = "O1"), excel)
  expect_equal(downloaded_kind(excel, "https://example.org/download?id=2"), "xlsx")

  csv <- tempfile()
  writeLines(c("occurrenceID,eventDate", "O1,2020-05-01"), csv)
  expect_equal(downloaded_kind(csv, "https://example.org/data/table.csv?raw=1"), "csv")
  expect_equal(downloaded_kind(csv, "https://example.org/data/table.txt"), "txt")
  expect_equal(downloaded_kind(csv, "https://ipt.example.org/archive.do?r=x"), "archive")   # an IPT gives archives only

  page <- tempfile()
  writeLines(c("<!DOCTYPE html>", "<html><body>Login</body></html>"), page)
  expect_error(downloaded_kind(page, "https://example.org/dataset"), "web page, not a dataset")
})

test_that("downloaded files get a readable name with the right extension", {
  expect_equal(downloaded_name("https://ipt.vliz.be/eurobis/archive.do?r=fifd", "archive"), "fifd.zip")
  expect_equal(downloaded_name("https://example.org/data/my%20table.csv?raw=1", "csv"), "my_table.csv")
  expect_equal(downloaded_name("https://example.org/download?id=7", "xlsx"), "download.xlsx")
})

test_that("a table behind a web address is read as a table (download replaced by a local copy)", {
  csv <- tempfile(fileext = ".csv")
  fwrite(data.table(occurrenceID = "O1", eventDate = "2020-05-01", scientificName = "Abra alba"), csv)
  local_mocked_bindings(download_with_limit = function(url, file, max_size_mb) { file.copy(csv, file); url },
                        remote_size_mb = function(url) NA_real_)
  dataset <- qc_read_url("https://example.org/data/survey.csv", mapping = c(scientificName = "scientificName"))
  expect_equal(dataset$source$type, "table")
  expect_equal(dataset$source$path, "https://example.org/data/survey.csv")
  expect_equal(dataset$source$name, "survey.csv")
  expect_equal(dataset$tables$occurrence$scientificName, "Abra alba")
})

test_that("only web addresses; addresses on the own network can be refused (option)", {
  expect_error(download_dataset("C:/data/secret.csv"), "must start with http")
  expect_equal(url_host("https://user@ipt.vliz.be:8080/x?r=1"), "ipt.vliz.be")
  expect_equal(is_private_address(c("10.1.2.3", "172.20.0.1", "192.168.1.1", "127.0.0.1", "169.254.169.254", "8.8.8.8", "::1")),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_silent(refuse_private_address("http://localhost/x"))                      # off by default
  withr::local_options(qctool.block_private_addresses = TRUE)
  expect_error(refuse_private_address("http://localhost/x"), "private network")
  expect_error(refuse_private_address("http://192.168.0.10/archive.zip"), "private network")
})

test_that("the unzipped size can be limited (option qctool.max_unzipped_mb; no limit by default)", {
  archive_zip <- tempfile(fileext = ".zip")
  zip::zipr(archive_zip, list.files(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1")), full.names = TRUE))
  expect_equal(qc_option("qctool.max_unzipped_mb"), Inf)
  expect_equal(nrow(qc_read_dwca(archive_zip)$problems), 0)
  withr::local_options(qctool.max_unzipped_mb = 1e-6)
  expect_match(qc_read_dwca(archive_zip)$problems$message, "more than the limit")
})

test_that("an IPT resource is downloaded and read", {
  skip_if_offline()
  dataset <- qc_read_url("https://ipt.vliz.be/upload/resource?r=satellitetracking-turtles-mediterranean")
  expect_equal(dataset$source$type, "url")
  expect_equal(dataset$source$name, "satellitetracking-turtles-mediterranean.zip")
  expect_true(all(c("event", "occurrence", "emof") %in% names(dataset$tables)))
})
