# The app's screens, driven without a browser (shiny::testServer)

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
skip_if_not_installed("leaflet")

test_that("the app's page can be built", {
  expect_s3_class(app_ui(), "bslib_page")
})

test_that("load: a table gets a column mapping, and the check fills the result", {
  path <- tempfile(fileext = ".csv")
  fwrite(data.table(Date = "2020-05-01", Latitude = "51.2", Longitude = "2.9", Species = "Abra alba", Remark = "x"), path)
  result <- shiny::reactiveVal(NULL)
  shiny::testServer(load_server, args = list(result = result), {
    session$setInputs(source = "file", online = FALSE,
                      file = data.frame(name = "my table.csv", size = file.size(path), type = "text/csv", datapath = path))
    expect_equal(columns()$column, c("Date", "Latitude", "Longitude", "Species", "Remark"))
    session$setInputs(term_1 = "eventDate", term_2 = "decimalLatitude", term_3 = "decimalLongitude",
                      term_4 = "scientificName", term_5 = "", table_type = "occurrence")
    session$setInputs(run = 1)
    expect_equal(result()$run$source_name, "my table.csv")
    mapping <- result()$dataset$mapping
    expect_equal(mapping[column == "Species", term], "scientificName")
    expect_true(is.na(mapping[column == "Remark", term]))     # '(not used)'
  })
})

test_that("load: a messy table (Windows-1252 text, repeated column names, empty rows) does not break the app", {
  # The browser drops the connection on text that is not valid UTF-8, so all text sent to it must be valid.
  path <- tempfile(fileext = ".csv")
  writeBin(charToRaw("Group,NameLatin,d13C,Group\r\nBirds,Puffinus gravis,-17.62\xb10.28,Birds\r\n,,,\r\n,Salps,-21.88,\r\n"), path)
  result <- shiny::reactiveVal(NULL)
  shiny::testServer(load_server, args = list(result = result), {
    session$setInputs(source = "file", online = FALSE,
                      file = data.frame(name = "SI species.csv", size = file.size(path), type = "text/csv", datapath = path))
    html <- output$mapping$html
    expect_true(validUTF8(html))
    expect_match(html, "-17.62±0.28", fixed = TRUE)                  # shown as '±'
    session$setInputs(run = 1)
    expect_s3_class(result(), "qc_result")
  })
  checked <- shiny::isolate(result())
  expect_false(failed(safely(qc_map_points(checked))))                   # no positions at all
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(checked)), {
    session$setInputs(severity = c("error", "warning", "info"))
    expect_true(validUTF8(output$checks_occurrence$html))
  })
})

test_that("tables in the app: invalid text made valid, web addresses become links, other text stays text", {
  messy <- data.table(value = c("-17.62\xb10.28", "<b>bold</b>"), link = c("https://www.marinespecies.org", "not a link"))
  safe <- utf8_safe(messy)
  expect_true(all(validUTF8(safe$value)))
  expect_false(validUTF8(messy$value[1]))                                # the data itself is unchanged
  links <- web_links(as.data.frame(safe))
  expect_match(links$data$link[1], '<a href="https://www.marinespecies.org" target="_blank"', fixed = TRUE)
  expect_equal(links$data$link[2], "not a link")
  expect_equal(links$escape, "value")                                    # '<b>bold</b>' is shown as text
  # Inside a longer text; punctuation stays outside; an address cut short stays text; HTML stays text.
  expect_equal(linkify(htmltools::htmlEscape("see https://obis.org/data, or <i>https://gbif.org/a…</i>")),
               paste0('see <a href="https://obis.org/data" target="_blank" rel="noopener">https://obis.org/data</a>, ',
                      "or &lt;i&gt;https://gbif.org/a…&lt;/i&gt;"))
})

test_that("load: without a file or address the app asks for one", {
  shiny::testServer(load_server, args = list(result = shiny::reactiveVal(NULL)), {
    session$setInputs(source = "url", url = "", run = 1)
    expect_match(output$message$html, "Choose a file or give a web address first")
  })
})

app_test_result <- function() {
  qc_check(make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), eventDate = c("2020-05-01", "21/05/2020"),
                       decimalLatitude = c("51.2", "0"), decimalLongitude = c("2.9", "0")),
    occurrence = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), occurrenceID = c("O1", "O2"),
                            scientificName = "Abra alba", occurrenceStatus = "present")), online = FALSE)
}

test_that("overview: verdict, essentials, data checks per table filtered by severity, metadata line", {
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_match(output$top$html, "Not ready for harvest")
    expect_match(output$top$html, "Essentials")
    expect_match(output$per_table$html, "Data checks per table")
    session$setInputs(severity = "error")
    expect_match(output$checks_event$html, "eventDate does not follow ISO 8601")
    expect_match(output$checks_event$html, "'21/05/2020' → 2020-05-21", fixed = TRUE)       # value next to its correction
    expect_match(output$checks_event$html, "datasetName, institutionCode", fixed = TRUE)    # missing columns named in one row
    session$setInputs(severity = "info")
    expect_no_match(output$checks_event$html, "eventDate does not follow ISO 8601")
    expect_match(output$bottom$html, "Metadata \\(EML\\)")
  })
})

test_that("overview: broken links between tables are shown right under the essentials", {
  broken <- qc_check(make_archive(
    event = data.frame(id = "E1", eventID = "E1"),
    occurrence = data.frame(id = c("E1", "E8"), occurrenceID = c("O1", "O2"), scientificName = "Abra alba")), online = FALSE)
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(broken)), {
    expect_match(output$top$html, "Links between tables")
    expect_match(output$top$html, "Record points to an event or occurrence that does not exist")
  })
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_match(output$top$html, "All links are intact")
  })
})

test_that("overview: values with several IDs are shown under the links", {
  mixed <- qc_check(make_archive(occurrence = data.frame(
    id = c("O1", "O2", "O3"), occurrenceID = c("O1", "O2", "O3"), scientificName = "Abra alba",
    scientificNameID = paste0("urn:lsid:marinespecies.org:taxname:", c(141433, 141433, 14143)))), online = FALSE)
  shiny::testServer(overview_server, args = list(result = shiny::reactiveVal(mixed)), {
    expect_match(output$top$html, "Values and their IDs")
    expect_match(output$top$html, "scientificName and scientificNameID do not go one-to-one")
  })
})

test_that("metadata: FAIR, the metadata issues and what the metadata says", {
  shiny::testServer(metadata_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_match(output$page$html, "Findable")
    expect_match(output$page$html, "What the metadata says")
  })
})

test_that("a screen that fails shows the message; the app keeps running", {
  e <- safely(stop("something broke"))
  expect_true(failed(e))
  expect_match(as.character(error_alert(e, "The map")), "The map could not be shown.*something broke")
  expect_error(safely(shiny::req(FALSE)), class = "shiny.silent.error")    # 'nothing to show yet' is let through
})

test_that("issues: a problem shows its records, a record shows everything about it", {
  checked <- app_test_result()
  shiny::testServer(issues_server, args = list(result = shiny::reactiveVal(checked)), {
    session$setInputs(severity = c("error", "warning", "info"), check = "eventdate_iso8601")
    expect_equal(nrow(problems()), 1)
    session$setInputs(problems_rows_selected = 1)
    expect_equal(records()$record_id, "E2")
    expect_match(output$records_title, "Records with this problem: 1")
    session$setInputs(records_rows_selected = 1)
    expect_match(output$record_title, "Record E2")
    expect_match(output$record$html, "Issues")
    # A selection left over from a longer list (before the severity changed) is no selection.
    session$setInputs(problems_rows_selected = 99)
    expect_match(output$records_title, "choose a problem")
  })
})

test_that("the licence column of the data is shown with the metadata, not with the data issues", {
  checked <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba",
                                                           license = "All rights reserved")), online = FALSE)
  expect_true("license_field_valid" %in% metadata_problems(checked)$check_id)
  expect_false("license_field_valid" %in% data_problems(checked)$check_id)
  shiny::testServer(metadata_server, args = list(result = shiny::reactiveVal(checked)), {
    expect_match(output$page$html, "Occurrence table, column license")
  })
})

test_that("explore > tables: the tables, the first rows, and a search in the whole table", {
  shiny::testServer(tables_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_match(output$structure$html, "occurrences are linked to an event")
    session$setInputs(table = "occurrence")
    expect_match(output$rows_note$html, "The first 2 of 2 rows")
    session$setInputs(column = "occurrenceID", how = "is", value = "O2", find = 1)
    expect_match(output$rows_note$html, "1 rows found")
    expect_equal(found()$rows$occurrenceID, "O2")
  })
})

test_that("map: problem points and the others", {
  shiny::testServer(map_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_true("coordinates_not_zero" %in% points()$problems$check_id)
    expect_match(output$note$html, "Records with a position problem")
  })
})

test_that("map: a dataset whose only position problem is a missing column (Munida, inapesca)", {
  checked <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba",
                                                           decimalLatitude = "51.2345", decimalLongitude = "2.9123")), online = FALSE)
  shiny::testServer(map_server, args = list(result = shiny::reactiveVal(checked)), {
    expect_false(failed(points()))
    expect_match(output$note$html, "Records with a position problem: 0")
  })
})

test_that("explore: notes, charts and tables for time, taxa, measurements and events", {
  skip_if_not_installed("ggplot2")
  shiny::testServer(explore_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    expect_match(output$time_note$html, "Records \\(occurrence\\) per month")
    expect_false(failed(taxa()))
    expect_match(output$taxa_note$html, "different taxa")
    expect_match(output$emof_note$html, "No measurements")
    expect_match(output$events_note$html, "Level 1")
    expect_match(output$time_plot, "records")                  # the interactive chart, with its hover texts
  })
})

test_that("downloads: Excel, CSV (zip) and JSON files are written", {
  shiny::testServer(download_server, args = list(result = shiny::reactiveVal(app_test_result())), {
    session$setInputs(max_rows = 100000, sep = ";")
    expect_match(file_stem(), "^test_archive_\\w+_qc$")     # the dataset's own name in the file names
    expect_true(file.size(output$xlsx) > 0)
    expect_setequal(zip::zip_list(output$csv)$filename, c("checks.csv", "issues.csv", "issues_grouped.csv", "run.json"))
    expect_true(jsonlite::validate(paste(readLines(output$json, warn = FALSE), collapse = "")))
  })
})

test_that("help: find a check, read about it; the manual and About the tool", {
  shiny::testServer(checks_server, args = list(result = shiny::reactiveVal(NULL)), {
    session$setInputs(search = "NERC label")
    session$elapse(500)                                      # the search waits 400 ms for typing to stop
    expect_true("measurementtype_matches_typeid" %in% found()$check_id)
    session$setInputs(list_rows_selected = which(found()$check_id == "measurementtype_matches_typeid"))
    expect_match(output$detail$html, "NERC Vocabulary Server")
    expect_match(output$detail$html, "checks_09_emof_nerc.R")
  })
  skip_if_not_installed("markdown")
  shiny::testServer(manual_server, {
    expect_match(output$text$html, "Adding a check")
  })
  shiny::testServer(about_tool_server, args = list(result = shiny::reactiveVal(NULL)), {
    expect_match(output$page$html, "Disclaimer")
    expect_match(output$page$html, "manual.obis.org")
    expect_match(output$page$html, "qc_rules.xlsx")             # where the rules workbook is
    expect_true(file.size(output$rules_file) > 0)               # and it can be downloaded
  })
})

test_that("explore: distributions, relationships and sexes", {
  skip_if_not_installed("ggplot2")
  checked <- qc_check(explore_test_archive(), online = FALSE)
  shiny::testServer(explore_server, args = list(result = shiny::reactiveVal(checked)), {
    session$setInputs(dist_variable = "field:decimalLatitude", dist_bins = 20, dist_log = FALSE, dist_taxon = "")
    expect_match(output$dist_note$html, "10 values")
    v <- variables()
    session$setInputs(rel_x = v[label == "Length (cm)", id], rel_y = v[label == "Weight (g)", id], rel_model = "power",
                      rel_threshold = 3, rel_taxon = "")
    expect_match(output$rel_note$html, "Orange: 1 records")
    expect_match(output$rel_plot, "O5")                          # hovering a point shows its record ID
    expect_match(output$dist_plot, "records")
    expect_match(output$sex_note$html, "Males, females")
  })
})

test_that("load: the web address field takes only http(s) addresses, not paths on the server", {
  shiny::testServer(load_server, args = list(result = shiny::reactiveVal(NULL)), {
    session$setInputs(source = "url", url = "C:/Users/someone/data.zip", run = 1)
    expect_match(output$message$html, "starts with http:// or https://")
  })
})

test_that("load: a table behind a web address gets its column mapping, then is checked", {
  csv <- tempfile(fileext = ".csv")
  fwrite(data.table(Date = "2020-05-01", Latitude = "51.2", Longitude = "2.9", Species = "Abra alba"), csv)
  local_mocked_bindings(download_with_limit = function(url, file, max_size_mb) { file.copy(csv, file); url },
                        remote_size_mb = function(url) NA_real_)
  result <- shiny::reactiveVal(NULL)
  shiny::testServer(load_server, args = list(result = result), {
    session$setInputs(source = "url", url = "https://example.org/survey.csv", online = FALSE, run = 1)
    expect_match(output$message$html, "leads to a table")
    expect_null(result())
    expect_equal(columns()$column, c("Date", "Latitude", "Longitude", "Species"))
    session$setInputs(term_1 = "eventDate", term_2 = "decimalLatitude", term_3 = "decimalLongitude",
                      term_4 = "scientificName", table_type = "occurrence", run = 2)
    expect_equal(result()$run$source, "https://example.org/survey.csv")
    expect_equal(result()$dataset$mapping[column == "Species", term], "scientificName")
  })
})

test_that("load: 'only some checks' follows an own rules workbook", {
  shiny::testServer(load_server, args = list(result = shiny::reactiveVal(NULL)), {
    session$setInputs(source = "url", url = "https://example.org/x.zip", single = "no_such_check", run = 1)
    expect_match(output$message$html, "not in the rules workbook you chose")
  })
})

test_that("map: tracks can be drawn", {
  checked <- qc_check(explore_test_archive(), online = FALSE)
  shiny::testServer(map_server, args = list(result = shiny::reactiveVal(checked)), {
    session$setInputs(tracks = TRUE, track_by = "top-level event", checks = character(), background = TRUE, eml_area = TRUE)
    expect_equal(unique(tracks()$track), "C1")
  })
})
