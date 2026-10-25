# =============================================================================
# Summaries of a result, for the report and the app (docs/design.md 4.10)
#
#   qc_essentials()  % of occurrences OK for taxonomy, position, date and
#                    occurrenceStatus - the four things every EurOBIS record needs
#   qc_fair()        the metadata checks per FAIR letter: "F 6/9 · A 3/4 · ..."
#   qc_record()      one record: its values, all its issues, its parent events
#                    and its measurements
#   qc_map_points()  positions for a map: problem records and a sample of the rest
#
# These only summarise the checks; they add no rules of their own.
# =============================================================================

essential_fields <- list(
  "Taxonomy"         = c("scientificName", "scientificNameID"),
  "Position"         = c("decimalLatitude", "decimalLongitude"),
  "Date"             = "eventDate",
  "occurrenceStatus" = "occurrenceStatus")

#' The essentials: are taxonomy, position, date and occurrenceStatus OK?
#'
#' For each essential, the share of occurrences without an error in its fields
#' (own or inherited from the event). An error on an event counts for all the
#' occurrences of that event that take the value from it. Only problems about
#' these fields alone count (a duplicate check that compares many fields does not).
#'
#' @param result A result of [qc_check()].
#' Checks about these fields that did not run (e.g. WoRMS could not be reached)
#' are named in `not_run`: the share then only counts the checks that ran, and
#' an essential that would be green is grey ("not fully checked").
#'
#' @return A data.table: essential, fields, occurrences, ok, with_warnings,
#'   ok_percent, light ("green": all OK, "orange": at least 90 %, "red", "grey":
#'   no column, or not fully checked), note, not_run.
#' @export
qc_essentials <- function(result) {
  dataset <- result$dataset
  occurrences <- dataset$tables$occurrence
  if (is.null(occurrences) || !nrow(occurrences)) {
    return(data.table(essential = names(essential_fields), fields = NA_character_, occurrences = 0L, ok = 0L,
                      with_warnings = 0L, ok_percent = NA_real_, light = "grey", note = "no occurrence table", not_run = ""))
  }
  n <- nrow(occurrences)
  problems <- result$issues_grouped[table %chin% c("occurrence", "event") & severity %chin% c("error", "warning")]
  problems[, fields_of_problem := strsplit(field, "; ", fixed = TRUE)]
  issue_rows <- result$issue_rows[problem %in% problems$problem]

  rbindlist(lapply(names(essential_fields), function(label) {
    fields <- essential_fields[[label]]
    available <- vapply(fields, function(f) field_available(dataset, "occurrence", f), logical(1))
    if (!all(available)) {
      return(data.table(essential = label, fields = paste(fields, collapse = ", "), occurrences = n, ok = 0L,
                        with_warnings = 0L, ok_percent = 0, light = "red",
                        note = paste("no column", paste(fields[!available], collapse = " / ")), not_run = ""))
    }
    # Problems about these fields only (not e.g. duplicate_occurrences, which lists all compared fields).
    concerned <- problems[vapply(fields_of_problem, function(f) all(f %in% fields), logical(1))]
    with_error <- occurrences_hit(dataset, concerned[severity == "error"], issue_rows, fields)
    with_warning <- occurrences_hit(dataset, concerned[severity == "warning"], issue_rows, fields) & !with_error
    ok <- sum(!with_error)
    percent <- round(100 * ok / n, 1)
    missing_checks <- checks_not_run_for(result, fields)
    light <- if (ok == n) "green" else if (percent >= 90) "orange" else "red"
    if (length(missing_checks) && light == "green") light <- "grey"
    data.table(essential = label, fields = paste(fields, collapse = ", "), occurrences = n, ok = ok,
               with_warnings = sum(with_warning), ok_percent = percent, light = light,
               note = if (length(missing_checks)) sprintf("not fully checked: %d checks did not run (%s)", length(missing_checks),
                                                          paste(missing_checks, collapse = ", ")) else "",
               not_run = paste(missing_checks, collapse = ", "))
  }))
}

# Titles of the checks about these fields (column 'fields' of the rules
# workbook) that did not run: check error, not performed or skipped.
checks_not_run_for <- function(result, fields) {
  checks <- result$checks
  if (!"fields" %in% names(checks)) return(character())
  about <- vapply(strsplit(checks$fields, ";", fixed = TRUE), function(f) any(trimws(f) %in% fields), logical(1))
  checks$title[about & checks$status %chin% not_run_statuses]
}

# Which occurrences are hit by these problems: issues on the occurrence itself,
# or on the event it belongs to or takes one of the fields from.
occurrences_hit <- function(dataset, chosen, issue_rows, fields) {
  n <- nrow(dataset$tables$occurrence)
  hit <- logical(n)
  if (!nrow(chosen)) return(hit)
  rows <- issue_rows[problem %in% chosen$problem]
  table_of_row <- chosen$table[match(rows$problem, chosen$problem)]
  hit[rows$row[table_of_row == "occurrence"]] <- TRUE
  event_rows <- unique(rows$row[table_of_row == "event"])
  if (length(event_rows)) {
    hit[occurrence_event_row(dataset) %in% event_rows] <- TRUE
    for (f in fields) {
      source <- inherited_value(dataset, "occurrence", f)
      hit[source$from_table %chin% "event" & source$from_row %in% event_rows] <- TRUE
    }
  }
  hit
}

#' The metadata checks per FAIR letter
#'
#' A quick indication, not a certification. Which check counts for which
#' letter: `inst/reference/fair_checks.csv`. Checks that did not run (not
#' applicable, not performed, ...) are listed but not counted.
#'
#' @param result A result of [qc_check()].
#' @return A list: `summary` (letter, passed, counted, text) and `checks`
#'   (letter, check_id, title, status, issues).
#' @export
qc_fair <- function(result) {
  letters_of <- reference_table("fair_checks.csv")[, .(letter, check_id)]
  checks <- merge(letters_of, result$checks[, .(check_id, title, status, issues)], by = "check_id", all.x = TRUE, sort = FALSE)
  checks[is.na(status), status := "not in the rules"]
  checks[, counted := status %chin% c("passed", "issues found")]
  checks <- checks[order(match(letter, c("F", "A", "I", "R")))]
  summary <- checks[, .(passed = sum(status == "passed"), counted = sum(counted)), by = letter]
  summary[, text := sprintf("%s %d/%d", letter, passed, counted)]
  list(summary = summary, checks = checks[, .(letter, check_id, title, status, issues)])
}

#' One record: its values, its issues, its parent events and its measurements
#'
#' @param result A result of [qc_check()].
#' @param table_name "event", "occurrence", "emof", ...
#' @param record_row The row number of the record in that table (the column
#'   `row` of [qc_issues()]).
#' @return A list: `values` (field, value), `issues` (all issues of this record
#'   and of the events above it; column `where`), `parents` (the events above
#'   it, nearest first: level, field, value) and `measurements` (its eMoF rows).
#' @export
qc_record <- function(result, table_name, record_row) {
  dataset <- result$dataset
  data <- dataset$tables[[table_name]]
  if (is.null(data) || record_row < 1 || record_row > nrow(data)) stop("No row ", record_row, " in the ", table_name, " table", call. = FALSE)

  parent_rows <- parent_event_rows(dataset, table_name, record_row)
  event_ids <- if (length(parent_rows)) column_or_na(dataset$tables$event, "eventID") else character()
  issues <- rbindlist(c(
    list(data.table(where = "this record", issues_of_record(result, table_name, record_row))),
    lapply(parent_rows, function(event_row) {
      data.table(where = paste("event above:", event_ids[event_row]), issues_of_record(result, "event", event_row))
    })))

  link <- link_values(dataset, table_name, record_row)
  link <- if (is.null(link)) NULL else data.table(field = names(link), value = link[[1]])
  list(values = rbind(link, record_values(data, record_row)),
       link_note = link_note(dataset, table_name, record_row),
       issues = issues,
       parents = rbindlist(lapply(seq_along(parent_rows), function(level) {
         data.table(level = level, record_values(dataset$tables$event, parent_rows[level]))
       })),
       measurements = record_measurements(dataset, table_name, record_row))
}

#' The links between the tables: are all IDs unique, and does every link point
#' to a record that exists?
#'
#' The checks of eventID, parentEventID, occurrenceID and the core links (the
#' id column of the archive) in one table: these broken links are the most
#' common real errors, and the records involved get lost at the harvest.
#'
#' @param result A [qc_check()] result.
#' @return One row per link check that applies to the dataset (and per table
#'   where it found something): check_id, problem (the check's title), table,
#'   records (with a broken link; 0 = all fine), severity, status.
#' @export
qc_links <- function(result) checks_in_short(result, link_check_ids)

#' Values and their IDs: does every value go with one ID, and every ID with
#' one value?
#'
#' The one-to-one checks in one table: scientificName and scientificNameID,
#' measurementType and measurementTypeID, measurementUnit and
#' measurementUnitID, measurementValue and measurementValueID (per type), and
#' the pairs of `inst/reference/one_to_one_pairs.csv` (institutionCode and
#' institutionID, country and countryCode, ...).
#'
#' @inheritParams qc_links
#' @return As [qc_links()]: one row per check (and table), with the records
#'   that use a less common combination (0 = all fine).
#' @export
qc_one_to_one <- function(result) checks_in_short(result, one_to_one_check_ids)

# The checks of the links between tables, in the order the overview shows them.
link_check_ids <- c("extension_links_to_core", "extension_eventid_matches_coreid", "extension_occurrenceid_matches_coreid",
                    "emof_occurrenceid_exists", "dna_occurrenceid_exists", "emof_event_matches_occurrence", "emof_record_linked",
                    "parenteventid_exists", "event_hierarchy_no_loops", "eventid_unique", "occurrenceid_unique", "measurementid_unique")

# The checks of values and their IDs, in the order the overview shows them.
one_to_one_check_ids <- c("scientificname_id_one_to_one", "measurementtype_id_one_to_one", "measurementunit_id_one_to_one",
                          "measurementvalueid_consistent", "values_and_ids_one_to_one")

# Some checks in short: one row per check (and per table where it found
# something), with the number of records involved.
checks_in_short <- function(result, ids) {
  applies <- result$checks[check_id %chin% ids & !status %chin% c("not applicable", "switched off", "not yet implemented")]
  if (!nrow(applies)) {
    return(data.table(check_id = character(), problem = character(), table = character(), records = numeric(),
                      severity = character(), status = character()))
  }
  # The severity of what was found (a check can report some problems as warning only).
  found <- result$issues_grouped[check_id %chin% applies$check_id,
                                 .(records = sum(as.numeric(pmax(records, 1L))), severity = severity[which.max(severity_rank[severity])]),
                                 by = .(check_id, table)]
  fine <- applies[!check_id %chin% found$check_id, .(check_id, table = NA_character_, records = 0, severity)]
  short <- rbind(found, fine)
  short <- short[order(match(check_id, ids), table)]
  short[, `:=`(problem = applies$title[match(check_id, applies$check_id)],
               status = applies$status[match(check_id, applies$check_id)])]
  short[, .(check_id, problem, table, records, severity, status)]
}

# In words, when a record's links are broken: an occurrence, measurement or DNA
# record pointing to an event (or occurrence) that does not exist, or an event
# whose parent does not exist. NA when the links are fine.
link_note <- function(dataset, table_name, record_row) {
  data <- dataset$tables[[table_name]]
  core <- dataset$core
  if (identical(table_name, "event") && "parentEventID" %in% names(data)) {
    parent <- data$parentEventID[record_row]
    if (!is.na(parent) && is.na(event_parent_row(dataset)[record_row])) {
      return(sprintf("Its parentEventID '%s' does not exist in the event table, so it takes no values from a parent event.", parent))
    }
    return(NA_character_)
  }
  if (is.na(core) || identical(table_name, core) || is.null(dataset$tables[[core]])) return(NA_character_)
  link <- data$.coreid[record_row]
  if (is.na(link)) return(sprintf("This %s is not linked to any %s: its id (core link) is empty.", record_word(table_name), core))
  if (!link %chin% dataset$tables[[core]]$.coreid) {
    return(sprintf("This %s points to %s '%s', which does not exist in the %s table.", record_word(table_name), core, link, core))
  }
  NA_character_
}

issues_of_record <- function(result, in_table, at_row) {
  problem_ids <- result$issue_rows[row == at_row, problem]
  result$issues_grouped[problem %in% problem_ids & table == in_table, .(check_id, severity, field, value, message, suggestion)]
}

# The filled fields of one row, as field / value.
record_values <- function(data, record_row) {
  columns <- data_columns(data)
  values <- vapply(columns, function(column) as.character(data[[column]][record_row]), character(1))
  data.table(field = columns, value = unname(values))[!is.na(value) & nzchar(value)]
}

# The rows of the events above a record, nearest first (at most 50 levels).
parent_event_rows <- function(dataset, table_name, record_row) {
  if (is.null(dataset$tables$event) || !table_name %chin% c("event", "occurrence")) return(integer())
  current <- if (table_name == "occurrence") occurrence_event_row(dataset)[record_row] else event_parent_row(dataset)[record_row]
  rows <- integer()
  while (length(current) == 1 && !is.na(current) && length(rows) < 50) {
    rows <- c(rows, current)
    current <- event_parent_row(dataset)[current]
  }
  rows
}

# The eMoF rows that belong to an occurrence, or directly to an event.
record_measurements <- function(dataset, table_name, record_row) {
  name <- emof_table_name(dataset)
  if (is.na(name) || !table_name %chin% c("event", "occurrence")) return(data.table())
  emof <- dataset$tables[[name]]
  rows <- if (table_name == "occurrence") {
    which(emof_occurrence_row(dataset) == record_row)
  } else {
    which(emof_event_row(dataset) == record_row & is.na(column_or_na(emof, "occurrenceID")))
  }
  columns <- intersect(c("measurementType", "measurementTypeID", "measurementValue", "measurementValueID",
                         "measurementUnit", "measurementUnitID", "measurementRemarks"), names(emof))
  emof[rows, ..columns]
}

#' Positions for a map: records with a position problem, and the other positions
#'
#' @param result A result of [qc_check()].
#' @param max_background Most positions without problems to return (a random
#'   sample above that, so a map stays quick).
#' @param max_per_problem Most records per problem.
#' @param max_problems Most problem points in all; above that errors go first,
#'   then warnings, then info, each a random sample (FIFD has 119,000).
#' @return A list: `problems` (lat, lon, check_id, severity, message, value,
#'   table, row, record_id), `background` (lat, lon), and `problems_total` and
#'   `background_total` (the numbers before sampling).
#' @export
qc_map_points <- function(result, max_background = 20000, max_per_problem = 2000, max_problems = 20000) {
  dataset <- result$dataset
  position_fields <- c("decimalLatitude", "decimalLongitude")
  tables <- intersect(c("event", "occurrence"), names(dataset$tables))
  tables <- tables[vapply(tables, function(t) all(vapply(position_fields, function(f) field_available(dataset, t, f), logical(1))), logical(1))]

  background <- rbindlist(c(list(data.table(lat = numeric(), lon = numeric())),   # also when no table has positions
                            lapply(tables, function(t) {
    data.table(lat = as_number(inherited_value(dataset, t, "decimalLatitude")$value),
               lon = as_number(inherited_value(dataset, t, "decimalLongitude")$value))
  })))
  background <- unique(background[!is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180])
  background_total <- nrow(background)
  if (background_total > max_background) background <- background[sample.int(.N, max_background)]

  no_points <- list(problems = data.table(problem = integer(), row = integer(), table = character(), lat = numeric(), lon = numeric(),
                                          record_id = character(), check_id = character(), severity = character(),
                                          message = character(), value = character()),
                    background = background, problems_total = 0L, background_total = background_total)
  # Problems with records (not e.g. "column maximumDepthInMeters is missing", which has no position).
  about_position <- result$issues_grouped[table %chin% tables & records > 0 &
                                            grepl("decimalLatitude|decimalLongitude|maximumDepthInMeters", field)]
  if (!nrow(about_position)) return(no_points)
  rows <- result$issue_rows[problem %in% about_position$problem]
  if (!nrow(rows)) return(no_points)
  rows <- rows[, .SD[seq_len(min(.N, max_per_problem))], by = problem]
  points <- rbindlist(lapply(split(rows, about_position$table[match(rows$problem, about_position$problem)]), function(part) {
    t <- about_position$table[match(part$problem[1], about_position$problem)]
    data.table(part, table = t,
               lat = as_number(inherited_value(dataset, t, "decimalLatitude")$value[part$row]),
               lon = as_number(inherited_value(dataset, t, "decimalLongitude")$value[part$row]),
               record_pointer(dataset, t, part$row)[, .(record_id)])
  }))
  details <- about_position[match(points$problem, about_position$problem)]
  points[, `:=`(check_id = details$check_id, severity = details$severity, message = details$message, value = details$value)]
  points <- points[!is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180]
  problems_total <- nrow(points)
  if (problems_total > max_problems) {
    points <- points[sample.int(.N)][order(-severity_rank[severity])][seq_len(max_problems)]
  }
  list(problems = points, background = background, problems_total = problems_total, background_total = background_total)
}
