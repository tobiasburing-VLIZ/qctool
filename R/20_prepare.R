# =============================================================================
# Links between tables, and values inherited from parent events
#
# In an event hierarchy (cruise > station > sample) values such as eventDate or
# the coordinates are often only written on a parent. OBIS fills them downwards
# ("flattening"); so does this tool, but WITHOUT changing the tables:
#
#   inherited_value(dataset, "occurrence", "eventDate")
#     -> for every occurrence: its own eventDate, or else the eventDate of its
#        event, or else of the nearest parent event that has one
#        ($value), plus WHERE that value is written ($from_table, $from_row)
#
# Own values always win; only empty fields inherit. Which fields inherit:
# inst/reference/inheritable_fields.csv (dates, positions, depths, places,
# datasetName, institutionCode, licence, ...).
#
# Links (row numbers), computed once per dataset and kept in dataset$cache:
#   event_parent_row(dataset)      each event -> row of its parent event
#   occurrence_event_row(dataset)  each occurrence -> row of its event
#   emof_event_row(dataset)        each eMoF record -> row of its event
#   emof_occurrence_row(dataset)   each eMoF record -> row of its occurrence
# NA means: no link (empty, or pointing to something that does not exist).
#
# Rule (docs/design.md, section 4.2): "is the value there?" uses the
# inherited value; "is the value right?" is checked where the value is written.
# =============================================================================

# Remember a result for this dataset (dataset$cache is shared by all copies).
cached <- function(dataset, key, compute) {
  if (is.null(dataset$cache)) return(compute)
  if (!exists(key, envir = dataset$cache, inherits = FALSE)) assign(key, compute, envir = dataset$cache)
  get(key, envir = dataset$cache, inherits = FALSE)
}

inheritable_fields <- function() reference_table("inheritable_fields.csv")$term

# --- Links ---------------------------------------------------------------------------

event_parent_row <- function(dataset) {
  cached(dataset, "event_parent_row", {
    events <- dataset$tables$event
    if (is.null(events)) integer()
    else if (!"parentEventID" %in% names(events)) rep(NA_integer_, nrow(events))
    else match(events$parentEventID, event_ids(dataset))
  })
}

# The archive's id column for some rows of a table, as a one-column table to
# show next to the data: 'id' in the core (the record's own id), 'id (core
# link)' in an extension (the core record it belongs to). NULL for simple
# tables, which have no id column.
link_values <- function(dataset, table_name, rows) {
  data <- dataset$tables[[table_name]]
  if (is.null(data) || all(is.na(data$.coreid))) return(NULL)
  name <- if (identical(dataset$core, table_name)) "id" else "id (core link)"
  setnames(data.table(data$.coreid[rows]), name)
}

# The ID of every event: its eventID, or else (event core) its core id, the
# 'id' column of the archive. Parents and links use these, so a missing eventID
# column does not hide broken parents.
event_ids <- function(dataset) {
  events <- dataset$tables$event
  if (is.null(events)) return(character())
  ids <- if ("eventID" %in% names(events)) events$eventID else rep(NA_character_, nrow(events))
  if (identical(dataset$core, "event")) ids[is.na(ids)] <- events$.coreid[is.na(ids)]
  ids
}

occurrence_event_row <- function(dataset) {
  cached(dataset, "occurrence_event_row", {
    link_rows(dataset, from = "occurrence", to = "event", id_column = "eventID")
  })
}

emof_event_row <- function(dataset) {
  cached(dataset, "emof_event_row", link_rows(dataset, from = emof_table_name(dataset), to = "event", id_column = "eventID"))
}

emof_occurrence_row <- function(dataset) {
  cached(dataset, "emof_occurrence_row", link_rows(dataset, from = emof_table_name(dataset), to = "occurrence", id_column = "occurrenceID"))
}

# "emof", or "mof" for the old MeasurementOrFact extension, or NA.
emof_table_name <- function(dataset) {
  name <- intersect(c("emof", "mof"), names(dataset$tables))
  if (length(name)) name[1] else NA_character_
}

# Rows of table 'to' that the records of table 'from' point to:
#   - through the core id (DwC-A link) when 'to' is the core
#   - otherwise through the id column (eventID / occurrenceID) when both tables have it
link_rows <- function(dataset, from, to, id_column) {
  source <- if (is.na(from)) NULL else dataset$tables[[from]]
  target <- dataset$tables[[to]]
  if (is.null(source)) return(integer())
  if (is.null(target)) return(rep(NA_integer_, nrow(source)))

  if (identical(dataset$core, to) && !identical(from, to) && any(!is.na(source$.coreid))) {
    return(match(source$.coreid, target$.coreid))
  }
  if (id_column %in% names(source) && id_column %in% names(target)) {
    return(match(source[[id_column]], target[[id_column]]))
  }
  rep(NA_integer_, nrow(source))
}

# --- Inherited values -------------------------------------------------------------

#' The value of a field for every record, with values inherited from parent events
#'
#' @param dataset A dataset.
#' @param table "event" or "occurrence" (other tables: own values only).
#' @param field A Darwin Core term, e.g. "eventDate".
#' @return A list: `value` (one per record), `from_table` and `from_row`
#'   (where each value is written; NA when there is no value at all).
#' @keywords internal
inherited_value <- function(dataset, table, field) {
  cached(dataset, paste("inherited", table, field, sep = "|"), {
    data <- dataset$tables[[table]]
    if (is.null(data)) stop("no ", table, " table")
    own <- if (field %in% names(data)) data[[field]] else rep(NA_character_, nrow(data))
    from_table <- ifelse(is.na(own), NA_character_, table)
    from_row <- ifelse(is.na(own), NA_integer_, seq_len(nrow(data)))
    result <- list(value = own, from_table = from_table, from_row = from_row)

    if (field %in% inheritable_fields() && any(is.na(own))) {
      if (table == "event") {
        result <- inherit_from_parents(dataset, field, result)
      } else if (table == "occurrence" && "event" %in% names(dataset$tables)) {
        result <- inherit_from_events(dataset, field, result)
      }
    }
    result
  })
}

# Events: walk up the hierarchy, one generation at a time, until every event
# has a value or there are no more parents. At most 50 generations, so a loop
# in the hierarchy (reported by event_hierarchy_no_loops) cannot hang the tool.
inherit_from_parents <- function(dataset, field, result) {
  events <- dataset$tables$event
  if (!field %in% names(events)) return(result)
  own <- events[[field]]
  parent <- event_parent_row(dataset)
  ancestor <- parent
  for (generation in seq_len(50)) {
    need <- which(is.na(result$value) & !is.na(ancestor))
    if (!length(need)) break
    found <- !is.na(own[ancestor[need]])
    take <- need[found]
    result$value[take] <- own[ancestor[take]]
    result$from_table[take] <- "event"
    result$from_row[take] <- ancestor[take]
    ancestor[need] <- parent[ancestor[need]]
  }
  result
}

# Occurrences: take the (inherited) value of their event.
inherit_from_events <- function(dataset, field, result) {
  event_values <- inherited_value(dataset, "event", field)
  event_row <- occurrence_event_row(dataset)
  need <- which(is.na(result$value) & !is.na(event_row))
  if (!length(need)) return(result)
  rows <- event_row[need]
  result$value[need] <- event_values$value[rows]
  result$from_table[need] <- event_values$from_table[rows]
  result$from_row[need] <- event_values$from_row[rows]
  result
}

# Events that are leaves of the hierarchy (no child events) or have occurrences:
# the events whose own information matters for the records.
events_that_hold_records <- function(dataset) {
  cached(dataset, "events_that_hold_records", {
    events <- dataset$tables$event
    if (is.null(events)) return(integer())
    is_parent <- seq_len(nrow(events)) %in% event_parent_row(dataset)
    has_occurrences <- seq_len(nrow(events)) %in% occurrence_event_row(dataset)
    which(!is_parent | has_occurrences)
  })
}

# --- Settings and rules for checks -------------------------------------------------

#' A setting of a check, with a default
#'
#' Checks read their settings with this function, so a setting that is missing
#' in the rules workbook falls back to the default written in the check.
#' @param settings The settings list a check receives.
#' @param name Setting name, e.g. "buffer_m".
#' @param default Value used when the workbook does not set it.
#' @keywords internal
setting <- function(settings, name, default) {
  value <- settings[[name]]
  if (is.null(value) || length(value) == 0 || all(is.na(value))) default else value
}

# Required / recommended fields (sheet 'Fields' of the rules workbook). The
# engine puts the rules in dataset$rules; without it (e.g. in tests) the
# package workbook is used.
field_requirements <- function(dataset) {
  if (!is.null(dataset$rules)) return(dataset$rules$fields)
  qc_read_rules()$fields
}
