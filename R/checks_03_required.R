# =============================================================================
# Checks, group "required": are the required fields there and filled?
#
#   required_fields_present        required_fields_filled
#   recommended_fields             modified_filled
#   datasetname_single_value       datasetname_equals_title
#   missing_values_found_elsewhere occurrencestatus_has_presence
#
# Which fields are required or recommended: sheet 'Fields' of the rules
# workbook (column 'requirement'). Rows with applies_to = 'dna-derived' are
# handled by dna_fields_complete.
#
# Where a missing value is reported (docs/design.md, section 4.2):
#   - fields that live on events (eventDate, coordinates, ...): on the EVENT
#     that lacks it, counting values inherited from parent events. Its
#     occurrences are not reported again.
#   - occurrences without an event: on the occurrence.
#   - fields of the occurrence or eMoF table: on that record.
# A placeholder such as "NA" counts as missing.
# =============================================================================

# -----------------------------------------------------------------------------
# CHECK  required_fields_present
# What:  every required column exists, in its own table or in a parent event.
# Why:   required fields are the minimum for a record to be usable.
# -----------------------------------------------------------------------------
check_required_fields_present <- function(dataset, settings) {
  missing_columns(dataset, requirement_rows(dataset, "required"),
                  "Required column is missing (not in this table and not in the event table)")
}

# -----------------------------------------------------------------------------
# CHECK  required_fields_filled
# What:  required fields have a value in every record, counting inherited values.
# Why:   same as above, record by record.
# -----------------------------------------------------------------------------
check_required_fields_filled <- function(dataset, settings) {
  empty_values(dataset, requirement_rows(dataset, "required"), "Required value")
}

# -----------------------------------------------------------------------------
# CHECK  recommended_fields
# What:  recommended fields are present and filled.
# Why:   they make the data much more useful.
# -----------------------------------------------------------------------------
check_recommended_fields <- function(dataset, settings) {
  fields <- requirement_rows(dataset, "recommended")
  combine_issues(list(
    missing_columns(dataset, fields, "Recommended column is missing"),
    empty_values(dataset, fields, "Recommended value")))
}

# -----------------------------------------------------------------------------
# CHECK  modified_filled
# What:  dcterms:modified (when the record was last changed) is there and
#        filled, in the event and occurrence tables that have the column.
# Why:   it shows which records changed between versions of a dataset.
#        Info for now (decision 2026-10-25); BioCheckTNG treats it as required.
#        (The sheet Fields has no 'info' level, hence this own check.)
# -----------------------------------------------------------------------------
check_modified_filled <- function(dataset, settings) {
  tables <- intersect(c("event", "occurrence"), names(dataset$tables))
  if (!length(tables)) skip_check("no event or occurrence table")
  with_column <- tables[vapply(tables, function(t) "modified" %in% names(dataset$tables[[t]]), logical(1))]
  if (!length(with_column)) {
    return(new_issues(dataset, tables[1], field = "modified",
                      message = "No modified column (dcterms:modified: when the record was last changed)"))
  }
  combine_issues(lapply(with_column, function(table_name) {
    values <- dataset$tables[[table_name]]$modified
    rows <- which(is_empty_value(values))
    new_issues(dataset, table_name, rows, field = "modified", value = values[rows],
               message = "modified (dcterms:modified: when the record was last changed) is empty")
  }))
}

# The 'all' rows of the sheet Fields with a given requirement.
# (Careful with data.table: inside fields[...] a name means the COLUMN first,
# so the argument must not be called 'requirement'.)
requirement_rows <- function(dataset, wanted) {
  fields <- field_requirements(dataset)
  fields[requirement == wanted & applies_to == "all"]
}

# The tables of 'checked_in' that this dataset has.
tables_for <- function(dataset, checked_in) {
  intersect(split_list(checked_in), names(dataset$tables))
}

# Is a field available for a table: its own column, or inherited from the event table?
field_available <- function(dataset, table, field) {
  field %in% names(dataset$tables[[table]]) ||
    (table == "occurrence" && field %in% inheritable_fields() && field %in% names(dataset$tables$event))
}

missing_columns <- function(dataset, fields, message) {
  combine_issues(lapply(seq_len(nrow(fields)), function(i) {
    tables <- tables_for(dataset, fields$checked_in[i])
    if (!length(tables)) return(NULL)
    available <- vapply(tables, field_available, logical(1), dataset = dataset, field = fields$term[i])
    if (any(available)) return(NULL)
    new_issues(dataset, tables[1], field = fields$term[i], message = message)
  }))
}

empty_values <- function(dataset, fields, label) {
  combine_issues(lapply(seq_len(nrow(fields)), function(i) {
    field <- fields$term[i]
    tables <- tables_for(dataset, fields$checked_in[i])
    if (!length(tables) || !any(vapply(tables, field_available, logical(1), dataset = dataset, field = field))) return(NULL)
    records_without(dataset, field, tables, label)
  }))
}

# Records without a value for one field, reported where it can be fixed.
records_without <- function(dataset, field, tables, label) {
  empty <- function(values) is.na(values) | is_placeholder(values)
  issues <- list()

  check_events <- "event" %in% tables && field %in% names(dataset$tables$event)
  if ("occurrence" %in% tables) {
    occurrence_value <- inherited_value(dataset, "occurrence", field)$value
    missing_occurrence <- empty(occurrence_value)
    event_row <- occurrence_event_row(dataset)
    # Occurrences of an event are covered by reporting that event.
    report_on_occurrence <- missing_occurrence & (is.na(event_row) | !check_events)
    # In an event core, an occurrence without an existing event cannot take the value from it: say so.
    not_linked <- identical(dataset$core, "event") & is.na(event_row)
    issues$occurrence <- empty_value_issues(dataset, "occurrence", which(report_on_occurrence), field, occurrence_value, label,
                                            not_linked = not_linked)
  }

  if (check_events) {
    event_value <- inherited_value(dataset, "event", field)$value
    # Fields only for events (eventID): every event. Fields for records: the events that hold records.
    candidates <- if (identical(tables, "event")) seq_along(event_value) else events_that_hold_records(dataset)
    rows <- candidates[empty(event_value[candidates])]
    if ("occurrence" %in% tables && length(rows)) {
      # An event whose occurrences all have their own value is not a problem.
      events_with_missing_occurrences <- unique(event_row[missing_occurrence])
      has_occurrences <- rows %in% event_row
      rows <- rows[!has_occurrences | rows %in% events_with_missing_occurrences]
    }
    # An event whose parentEventID points nowhere cannot take the value from a parent: say so.
    events <- dataset$tables$event
    no_parent <- if ("parentEventID" %in% names(events)) !is.na(events$parentEventID) & is.na(event_parent_row(dataset)) else FALSE
    issues$event <- empty_value_issues(dataset, "event", rows, field, event_value, label, not_linked = no_parent,
                                       unlinked_text = "its parent event does not exist, so it cannot take the value from it")
  }

  for (table in setdiff(tables, c("event", "occurrence"))) {
    values <- dataset$tables[[table]][[field]]
    if (is.null(values)) next
    issues[[table]] <- empty_value_issues(dataset, table, which(empty(values)), field, values, label)
  }
  combine_issues(issues)
}

empty_value_issues <- function(dataset, table, rows, field, values, label, not_linked = FALSE,
                               unlinked_text = "the occurrence is not linked to an existing event, so it cannot take the value from it") {
  if (!length(rows)) return(empty_issues())
  placeholder <- !is.na(values[rows])
  unlinked <- rep_len(not_linked, length(values))[rows]
  new_issues(dataset, table, rows, field = field,
             value = ifelse(placeholder, values[rows], NA_character_),
             message = ifelse(placeholder, paste(label, "is only a placeholder"),
                       ifelse(unlinked, paste0(label, " is missing: ", unlinked_text),
                              paste(label, "is missing (also not inherited from a parent event)"))))
}

# -----------------------------------------------------------------------------
# CHECK  datasetname_single_value
# What:  datasetName has only one distinct value in the whole dataset.
# Why:   a dataset has one name; several values point to merged datasets or typos.
# -----------------------------------------------------------------------------
check_datasetname_single_value <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "datasetName")
  if (!length(tables)) skip_check("no datasetName column")
  all_names <- unique(unlist(lapply(tables, function(t) dataset$tables[[t]]$datasetName)))
  all_names <- all_names[!is.na(all_names)]
  if (length(all_names) <= 1) return(empty_issues())
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$datasetName
    rows <- which(!is.na(values))
    new_issues(dataset, table_name, rows, field = "datasetName", value = values[rows],
               message = sprintf("datasetName has %d different values in this dataset", length(all_names)))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  datasetname_equals_title
# What:  datasetName is the same as the title in the metadata (EML).
# Why:   EMODnet asks for the datasetName to be the dataset title.
# -----------------------------------------------------------------------------
check_datasetname_equals_title <- function(dataset, settings) {
  title <- dataset$metadata$title
  if (is.null(title) || is.na(title)) skip_check("no dataset title in the metadata")
  tables <- tables_with_fields(dataset, "datasetName")
  if (!length(tables)) skip_check("no datasetName column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$datasetName
    rows <- which(!is.na(values) & per_value(values, trimws) != title)
    if (!length(rows)) return(empty_issues())
    similarity <- round(100 * text_similarity(values[rows], title))
    new_issues(dataset, table_name, rows, field = "datasetName", value = values[rows],
               message = sprintf("datasetName differs from the dataset title in the metadata (similarity %d%%)", similarity),
               suggestion = title)
  }))
}

# 1 = identical, 0 = nothing in common (based on the number of letter edits).
text_similarity <- function(a, b) {
  distance <- as.numeric(utils::adist(a, b))
  1 - distance / pmax(nchar(a), nchar(b))
}

# -----------------------------------------------------------------------------
# CHECK  missing_values_found_elsewhere
# What:  when eventDate, coordinates or depth are missing, looks for them in
#        other fields of the same record and shows what was found.
# Why:   turns 'value missing' into a concrete fix suggestion.
# -----------------------------------------------------------------------------
check_missing_values_found_elsewhere <- function(dataset, settings) {
  searches <- list(
    eventDate = date_like_pattern,
    decimalLatitude = "(?i)lat[a-z()]*[^0-9-]{0,25}(-?\\d+\\.\\d+)",
    decimalLongitude = "(?i)lon[a-z()]*[^0-9-]{0,25}(-?\\d+\\.\\d+)",
    minimumDepthInMeters = "(?i)depth[^0-9-]{0,25}(\\d+(\\.\\d+)?)")
  places <- c("eventRemarks", "verbatimEventDate", "verbatimLocality", "verbatimCoordinates", "verbatimLatitude",
              "verbatimLongitude", "verbatimDepth", "locality", "dynamicProperties", "occurrenceRemarks", "fieldNotes")

  combine_issues(lapply(names(searches), function(field) {
    tables <- intersect(c("event", "occurrence"), names(dataset$tables))
    if (!length(tables)) return(NULL)
    missing <- records_without(dataset, field, tables, "value")
    if (!nrow(missing$problems)) return(NULL)
    combine_issues(lapply(unique(missing$problems$table), function(table_name) {
      rows <- missing$rows$row[missing$problems$table[missing$rows$problem] == table_name]
      data <- dataset$tables[[table_name]]
      found <- rep(NA_character_, length(rows))
      for (place in intersect(places, names(data))) {
        texts <- data[[place]][rows]
        look <- which(is.na(found) & !is.na(texts) & validUTF8(texts))
        if (!length(look)) next
        position <- regexpr(searches[[field]], texts[look], perl = TRUE)
        matched <- look[position > 0]
        found[matched] <- sprintf("%s: '%s'", place, regmatches(texts[look], position))
      }
      keep <- !is.na(found)
      new_issues(dataset, table_name, rows[keep], field = field, value = found[keep],
                 message = sprintf("%s is missing, but a value seems to be written in another field", field),
                 suggestion = paste("move to", field, "(in ISO 8601 / decimal degrees / metres)"))
    }))
  }))
}

# Dates written in other forms: 2013-06-04, Jan2008, 04/06/2013, June 2013.
date_like_pattern <- paste0("(?i)(\\b(1[6-9]|20)\\d{2}-\\d{2}(-\\d{2})?\\b|",
                            "\\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[ ._-]?(1[6-9]|20)\\d{2}\\b|",
                            "\\b\\d{1,2}[/.]\\d{1,2}[/.](1[6-9]|20)\\d{2}\\b)")

# -----------------------------------------------------------------------------
# CHECK  occurrencestatus_has_presence
# What:  at least one occurrence has occurrenceStatus 'present' (or 'detected').
# Why:   a dataset with only absences is very unusual.
# -----------------------------------------------------------------------------
check_occurrencestatus_has_presence <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "occurrenceStatus")
  status <- lower_trim(occurrences$occurrenceStatus)
  if (any(status %in% c("present", "detected"))) return(empty_issues())
  new_issues(dataset, "occurrence", field = "occurrenceStatus",
             value = paste(utils::head(unique(without_na(occurrences$occurrenceStatus)), 5), collapse = ", "),
             message = "No occurrence has occurrenceStatus 'present': a dataset with only absences is very unusual")
}
