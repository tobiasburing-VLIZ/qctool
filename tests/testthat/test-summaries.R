# Summaries for the report and the app: essentials, FAIR, one record, map points

summary_test_archive <- function() {
  make_archive(
    event = data.frame(id = c("E1", "E2", "E3"), eventID = c("E1", "E2", "E3"), eventDate = c("2020-05-01", "21/05/2020", "2020-05-03"),
                       decimalLatitude = c("51.2345", "51.3456", "0"), decimalLongitude = c("2.9123", "3.0123", "0")),
    occurrence = data.frame(id = c("E1", "E2", "E2", "E3"), eventID = c("E1", "E2", "E2", "E3"), occurrenceID = c("O1", "O2", "O3", "O4"),
                            scientificName = "Abra alba", scientificNameID = lsid(141433), occurrenceStatus = "present"),
    emof = data.frame(id = "E1", occurrenceID = "O1", measurementType = "abundance", measurementValue = "3"))
}

test_that("essentials count occurrences without errors, also when the error is on their event", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  essentials <- qc_essentials(result)
  expect_equal(essentials$essential, c("Taxonomy", "Position", "Date", "occurrenceStatus"))
  expect_equal(essentials[essential == "Date", ok], 2L)              # O2 and O3 take '21/05/2020' from E2
  expect_equal(essentials[essential == "Date", light], "red")
  expect_equal(essentials[essential == "Position", with_warnings], 1L)   # O4 takes 0/0 from E3 (a warning)
  expect_equal(essentials[essential == "occurrenceStatus", light], "green")
})

test_that("essentials say so when a column is missing", {
  result <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba")), online = FALSE)
  position <- qc_essentials(result)[essential == "Position"]
  expect_equal(position$ok_percent, 0)
  expect_match(position$note, "no column")
})

test_that("FAIR: metadata checks per letter; checks that did not run are not counted", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  fair <- qc_fair(result)
  expect_equal(fair$summary$letter, c("F", "A", "I", "R"))
  expect_true(all(fair$summary$passed <= fair$summary$counted))
  expect_match(fair$summary$text[1], "^F \\d+/\\d+$")
  expect_true("eml_title_filled" %in% fair$checks$check_id)
})

test_that("one record: its values, its issues and those of its event, its event, its measurements", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  o2 <- qc_record(result, "occurrence", 2)
  expect_equal(o2$values[field == "occurrenceID", value], "O2")
  expect_true("eventdate_iso8601" %in% o2$issues[where == "event above: E2", check_id])
  expect_equal(unique(o2$parents$level), 1L)
  expect_equal(o2$parents[field == "eventID", value], "E2")
  expect_equal(nrow(qc_record(result, "occurrence", 1)$measurements), 1)
  expect_error(qc_record(result, "occurrence", 99), "No row 99")
})

test_that("explore: time, taxa, measurements and event levels", {
  result <- qc_check(summary_test_archive(), online = FALSE)

  time <- qc_time_summary(result)
  expect_equal(time$unit, "month")
  expect_equal(sum(time$counts$records), 2)                 # O1 and O4; O2/O3 have '21/05/2020' (not a valid date)
  expect_equal(time$undated, 2)

  taxa <- qc_taxa_summary(result)
  expect_equal(taxa$records, 4)
  expect_equal(taxa$scientificName, "Abra alba")

  emof <- qc_emof_summary(result)
  expect_equal(emof$measurementType, "abundance")
  expect_equal(c(emof$records, emof$numbers, emof$max), c(1, 1, 3))

  levels <- qc_event_levels(result)
  expect_equal(levels$level, "1")
  expect_equal(c(levels$events, levels$occurrences), c(3, 4))

  table_only <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba")), online = FALSE)
  expect_null(qc_emof_summary(table_only))
  expect_null(qc_event_levels(table_only))
  expect_null(qc_time_summary(table_only))
})

test_that("map points: records with a position problem, and all positions", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  points <- qc_map_points(result)
  expect_true("coordinates_not_zero" %in% points$problems$check_id)
  expect_equal(points$problems[check_id == "coordinates_not_zero", record_id], "E3")
  expect_equal(nrow(points$background), 3)

  # A position problem without records (a missing column) gives no points, and no error (Munida, inapesca).
  no_depth <- qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1", scientificName = "Abra alba",
                                                            decimalLatitude = "51.2345", decimalLongitude = "2.9123")), online = FALSE)
  expect_equal(nrow(qc_map_points(no_depth)$problems), 0)

  capped <- qc_map_points(result, max_problems = 1, max_background = 2)
  expect_equal(nrow(capped$problems), 1)                  # a map stays quick: FIFD has 119,000
  expect_equal(capped$problems_total, nrow(points$problems))
  expect_equal(c(nrow(capped$background), capped$background_total), c(2, 3))
})

test_that("explore more: numeric variables, a length-weight fit, sexes and tracks", {
  result <- qc_check(explore_test_archive(), online = FALSE)
  variables <- qc_numeric_variables(result, min_values = 5)
  expect_true(all(c("field:decimalLatitude", "year") %in% variables$id))
  length_id <- variables[label == "Length (cm)", id]
  weight_id <- variables[label == "Weight (g)", id]
  expect_equal(nrow(qc_variable_values(result, length_id, variables)), 10)

  fit <- qc_relationship(result, length_id, weight_id, model = "power", variables = variables)
  expect_match(fit$formula, "x^", fixed = TRUE)
  expect_equal(fit$points[far == TRUE, owner], 5L)                     # O5 only
  expect_null(qc_relationship(result, length_id, weight_id, taxon = "Abra alba", variables = variables))

  boxes <- qc_box_stats(qc_variable_values(result, length_id, variables))
  expect_equal(boxes$group[1], "all records")
  expect_equal(boxes$n[1], 10)
  expect_true(all(boxes$lower <= boxes$q1 & boxes$q1 <= boxes$median & boxes$median <= boxes$q3 & boxes$q3 <= boxes$upper))
  expect_equal(qc_box_stats(data.table(value = c(1:9, 100), taxon = "Abra alba"))$beyond, 1)   # 100 lies beyond the whisker
  skip_if_not_installed("plotly")
  expect_s3_class(box_chart(qc_variable_values(result, length_id, variables), on_log = TRUE, label = "Length (cm)"), "plotly")

  sexes <- qc_sex_ratio(result)
  expect_equal(c(sexes$female, sexes$male, sexes$other, sexes$share_female), c(4, 4, 2, 0.5))

  expect_true("top-level event" %in% track_groupings(result))
  tracks <- qc_tracks(result, "top-level event")
  expect_equal(unique(tracks$track), "C1")
  expect_equal(tracks$date, sort(tracks$date))                         # in date order
})

test_that("qc_map: an interactive map, a web page, or a picture of the positions with problems", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  skip_if_not_installed("leaflet")
  expect_s3_class(qc_map(result), "leaflet")
  folder <- withr::local_tempdir()
  expect_message(qc_map(result, file.path(folder, "map.html")), "Saved")
  expect_true(file.exists(file.path(folder, "map.html")))
  expect_message(qc_map(result, file.path(folder, "map.png"), severity = "warning"), "Saved")
  expect_gt(file.size(file.path(folder, "map.png")), 1000)
  expect_message(qc_map(result, file.path(folder, "map.pdf"), others = FALSE), "Saved")
})

test_that("links between tables: one overview of all link checks; a record says when its link is broken", {
  result <- qc_check(make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), parentEventID = c(NA, "E9")),
    occurrence = data.frame(id = c("E1", "E8"), occurrenceID = c("O1", "O2"), scientificName = "Abra alba"),
    emof = data.frame(id = c("E1", "E1"), occurrenceID = c("O1", "O5"), measurementType = "x")), online = FALSE)
  links <- qc_links(result)
  broken <- links[records > 0]
  expect_equal(broken$check_id, c("extension_links_to_core", "emof_occurrenceid_exists", "parenteventid_exists"))
  expect_equal(broken$table, c("occurrence", "emof", "event"))
  expect_true(all(links[records == 0, status] == "passed"))
  expect_equal(nrow(qc_links(qc_check(make_archive(occurrence = data.frame(id = "O1", occurrenceID = "O1")), online = FALSE))[records > 0]), 0)

  orphan <- qc_record(result, "occurrence", 2)
  expect_match(orphan$link_note, "points to event 'E8', which does not exist")
  expect_equal(orphan$values[field == "id (core link)", value], "E8")      # the link is shown with the values
  expect_match(qc_record(result, "event", 2)$link_note, "parentEventID 'E9' does not exist")
  expect_true(is.na(qc_record(result, "occurrence", 1)$link_note))
  expect_equal(names(qc_find_rows(result, "occurrence", how = "first")$rows)[2], "id (core link)")
})

test_that("tables: how they are linked, their columns, rows found in the whole table", {
  result <- qc_check(summary_test_archive(), online = FALSE)
  tables <- qc_tables(result)
  expect_equal(tables$role, c("core", "extension", "extension"))
  expect_match(tables[table == "Occurrence", `linked to`], "4 of 4 occurrences are linked to an event")
  expect_match(tables[table == "eMoF (measurements)", `linked to`], "1 measurements of an occurrence")
  columns <- qc_table_columns(result, "event")
  expect_equal(columns[column == "eventDate", filled], 3)
  expect_equal(qc_find_rows(result, "occurrence", "eventID", "E2", how = "is")$total, 2)
  expect_equal(qc_find_rows(result, "occurrence", "occurrenceID", "o3", how = "contains")$rows$occurrenceID, "O3")
  expect_equal(qc_find_rows(result, "occurrence", how = "line", value = "3")$rows$occurrenceID, "O2")
  expect_equal(nrow(qc_find_rows(result, "occurrence", how = "first", limit = 2)$rows), 2)
  expect_error(qc_find_rows(result, "occurrence", "nonsense", "x", how = "is"), "No column")
})

test_that("the check catalogue finds checks by words", {
  found <- qc_checks("NERC label")
  expect_true("measurementtype_matches_typeid" %in% found$check_id)
  expect_true(all(c("what", "why", "internet", "code") %in% names(found)))
  expect_match(found[check_id == "measurementtype_matches_typeid", code], "checks_09_emof_nerc.R")
  expect_equal(nrow(qc_checks("no such words anywhere")), 0)
  expect_equal(nrow(qc_checks()), nrow(qc_read_rules()$checks))
})
