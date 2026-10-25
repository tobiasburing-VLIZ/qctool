# =============================================================================
# Checks, group "identifiers": are the IDs unique, and do the links between
# events, occurrences and measurements point to records that exist?
#
#   eventid_unique                    parenteventid_exists
#   event_hierarchy_no_loops          extension_links_to_core
#   extension_eventid_matches_coreid  extension_occurrenceid_matches_coreid
#   occurrenceid_unique               occurrenceid_globally_unique
#   emof_occurrenceid_exists          dna_occurrenceid_exists
#   emof_event_matches_occurrence     emof_record_linked
#   measurementid_unique              eventid_equals_occurrenceid
#
# Links between tables: R/20_prepare.R. In a DwC archive the link from an
# extension to the core is the core id column (.coreid, shown as 'id (core
# link)'), not an eventID or occurrenceID column. The overview of all links:
# qc_links() in R/45_summaries.R.
# =============================================================================

# Rows whose value occurs more than once (all copies), ignoring empty values.
duplicated_rows <- function(values) {
  which(!is.na(values) & (duplicated(values) | duplicated(values, fromLast = TRUE)))
}

# The ID of every occurrence: its occurrenceID, or else (occurrence core) its core id.
occurrence_ids <- function(dataset) {
  occurrences <- dataset$tables$occurrence
  if (is.null(occurrences)) return(character())
  ids <- if ("occurrenceID" %in% names(occurrences)) occurrences$occurrenceID else rep(NA_character_, nrow(occurrences))
  if (identical(dataset$core, "occurrence")) ids[is.na(ids)] <- occurrences$.coreid[is.na(ids)]
  ids
}

# What one record of a table is called in messages.
record_word <- function(table_name) {
  words <- c(event = "event", occurrence = "occurrence", emof = "measurement", mof = "measurement", dna = "DNA record")
  ifelse(table_name %in% names(words), words[table_name], "record")
}

# -----------------------------------------------------------------------------
# CHECK  eventid_unique
# What:  no two events share the same eventID (or, without an eventID column,
#        the same id).
# Why:   eventID is the key of the event table; occurrences and measurements link to it.
# -----------------------------------------------------------------------------
check_eventid_unique <- function(dataset, settings) {
  events <- require_table(dataset, "event")
  ids <- event_ids(dataset)
  rows <- duplicated_rows(ids)
  new_issues(dataset, "event", rows, field = if ("eventID" %in% names(events)) "eventID" else "id (core link)", value = ids[rows],
             message = "eventID is used by more than one event, so records cannot be linked to one of them")
}

# -----------------------------------------------------------------------------
# CHECK  parenteventid_exists
# What:  every filled parentEventID exists as an eventID (empty is fine).
#        Without an eventID column the id column of the event core counts.
# Why:   a missing parent breaks the hierarchy and the inheritance of dates and positions.
# -----------------------------------------------------------------------------
check_parenteventid_exists <- function(dataset, settings) {
  events <- require_fields(dataset, "event", "parentEventID")
  rows <- which(!is.na(events$parentEventID) & !events$parentEventID %chin% event_ids(dataset))
  new_issues(dataset, "event", rows, field = "parentEventID", value = events$parentEventID[rows],
             message = "parentEventID points to an event that does not exist in the event table")
}

# -----------------------------------------------------------------------------
# CHECK  event_hierarchy_no_loops
# What:  no event is its own parent or its own ancestor (A > B > A).
# Why:   loops make inheritance impossible and crash tools that follow the hierarchy.
# How:   follows every chain of parents up to 100 generations.
# -----------------------------------------------------------------------------
check_event_hierarchy_no_loops <- function(dataset, settings) {
  events <- require_fields(dataset, "event", "parentEventID")
  parent <- event_parent_row(dataset)
  in_loop <- logical(nrow(events))
  ancestor <- parent
  for (generation in seq_len(100)) {
    in_loop[which(ancestor == seq_along(ancestor))] <- TRUE
    if (all(is.na(ancestor))) break
    ancestor <- parent[ancestor]
  }
  rows <- which(in_loop)
  new_issues(dataset, "event", rows, field = "parentEventID", value = events$parentEventID[rows],
             message = "The event is its own ancestor: the parentEventIDs form a loop")
}

# -----------------------------------------------------------------------------
# CHECK  extension_links_to_core
# What:  every extension record refers, through the core id (the archive's id
#        column), to an existing core record: an occurrence, measurement or DNA
#        record to an existing event (event core) or occurrence (occurrence core).
# Why:   records pointing to a non-existing event or occurrence are orphans and get lost.
# -----------------------------------------------------------------------------
check_extension_links_to_core <- function(dataset, settings) {
  extensions <- setdiff(names(dataset$tables), dataset$core)
  if (is.na(dataset$core) || !length(extensions)) skip_check("no extension tables")
  core_ids <- dataset$tables[[dataset$core]]$.coreid
  core_word <- if (identical(dataset$core, "event")) "an event" else "an occurrence"
  combine_issues(lapply(extensions, function(table_name) {
    links <- dataset$tables[[table_name]]$.coreid
    rows <- which(is.na(links) | !links %chin% core_ids)
    new_issues(dataset, table_name, rows, field = "id (core link)", value = links[rows],
               message = ifelse(is.na(links[rows]),
                                sprintf("The %s is not linked to %s: its id (core link) is empty", record_word(table_name), core_word),
                                sprintf("The %s points to %s that does not exist in the %s table", record_word(table_name), core_word, dataset$core)))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  extension_eventid_matches_coreid
# What:  if an extension (event core) has its own eventID column, it names an
#        existing event, and the same event the record is linked to (core id).
# Why:   an eventID that names a missing event, or another event than the link,
#        sends the record to the wrong place or nowhere.
# -----------------------------------------------------------------------------
check_extension_eventid_matches_coreid <- function(dataset, settings) {
  if (!identical(dataset$core, "event")) skip_check("the core is not an event table")
  extensions <- setdiff(tables_with_fields(dataset, "eventID"), "event")
  if (!length(extensions)) skip_check("no extension with an eventID column")
  known <- event_ids(dataset)
  combine_issues(lapply(extensions, function(table_name) {
    data <- dataset$tables[[table_name]]
    # (Records without a link at all are reported by extension_links_to_core.)
    rows <- which(!is.na(data$eventID) & !is.na(data$.coreid) & data$eventID != data$.coreid)
    new_issues(dataset, table_name, rows, field = "eventID", value = data$eventID[rows],
               message = ifelse(data$eventID[rows] %chin% known,
                                sprintf("The %s's eventID names another event than the one it is linked to (id column)", record_word(table_name)),
                                sprintf("The %s's eventID names an event that does not exist in the event table", record_word(table_name))),
               suggestion = paste("linked through the id column to:", data$.coreid[rows]))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  extension_occurrenceid_matches_coreid
# What:  in an occurrence core, an extension's own occurrenceID column (eMoF,
#        DNA) names an existing occurrence, and the one the record is linked to.
# Why:   a measurement or DNA record must belong to exactly one, existing occurrence.
# -----------------------------------------------------------------------------
check_extension_occurrenceid_matches_coreid <- function(dataset, settings) {
  if (!identical(dataset$core, "occurrence")) skip_check("the core is not an occurrence table")
  extensions <- setdiff(tables_with_fields(dataset, "occurrenceID"), "occurrence")
  if (!length(extensions)) skip_check("no extension with an occurrenceID column")
  known <- occurrence_ids(dataset)
  combine_issues(lapply(extensions, function(table_name) {
    data <- dataset$tables[[table_name]]
    # (Records without a link at all are reported by extension_links_to_core.)
    rows <- which(!is.na(data$occurrenceID) & !is.na(data$.coreid) & data$occurrenceID != data$.coreid)
    new_issues(dataset, table_name, rows, field = "occurrenceID", value = data$occurrenceID[rows],
               message = ifelse(data$occurrenceID[rows] %chin% known,
                                sprintf("The %s's occurrenceID names another occurrence than the one it is linked to (id column)", record_word(table_name)),
                                sprintf("The %s's occurrenceID names an occurrence that does not exist in the Occurrence table", record_word(table_name))),
               suggestion = paste("linked through the id column to:", data$.coreid[rows]))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  occurrenceid_unique
# What:  no two occurrences share the same occurrenceID.
# Why:   occurrenceID is the key of the occurrence table.
# -----------------------------------------------------------------------------
check_occurrenceid_unique <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "occurrenceID")
  rows <- duplicated_rows(occurrences$occurrenceID)
  new_issues(dataset, "occurrence", rows, field = "occurrenceID", value = occurrences$occurrenceID[rows],
             message = "occurrenceID is used by more than one occurrence")
}

# -----------------------------------------------------------------------------
# CHECK  occurrenceid_globally_unique   (switched off in the EMODnet rules)
# What:  occurrenceIDs are not very short codes (e.g. 'A1').
# Why:   IDs that stay unique when datasets are combined are easier to trace.
#        EurOBIS only requires an occurrenceID to be unique within the dataset
#        (occurrenceid_unique), so running numbers (1, 2, 3) are accepted and
#        never reported here.
# Settings: min_length (default 6)
# -----------------------------------------------------------------------------
check_occurrenceid_globally_unique <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "occurrenceID")
  min_length <- setting(settings, "min_length", 6)
  ids <- occurrences$occurrenceID
  too_short <- rows_where(ids, function(v) nchar(trimws(v)) < min_length & !grepl("^\\s*\\d+\\s*$", v))
  new_issues(dataset, "occurrence", too_short, field = "occurrenceID", value = sprintf("shorter than %d characters", min_length),
             message = "occurrenceID is very short, so probably not unique outside this dataset",
             suggestion = "add a prefix, e.g. institutionCode:collectionCode:number")
}

# -----------------------------------------------------------------------------
# CHECK  emof_occurrenceid_exists
# What:  (event core) every filled occurrenceID in the eMoF exists in the
#        Occurrence table. In an occurrence core this is covered by
#        extension_links_to_core and extension_occurrenceid_matches_coreid.
# Why:   measurements linked to a non-existing occurrence are lost.
# -----------------------------------------------------------------------------
check_emof_occurrenceid_exists <- function(dataset, settings) {
  emof_name <- emof_table_name(dataset)
  if (is.na(emof_name)) skip_check("no eMoF table")
  occurrenceid_issues(dataset, emof_name)
}

# -----------------------------------------------------------------------------
# CHECK  dna_occurrenceid_exists
# What:  (event core) every filled occurrenceID in the DNA-derived data exists
#        in the Occurrence table.
# Why:   DNA data of a non-existing occurrence cannot be used.
# -----------------------------------------------------------------------------
check_dna_occurrenceid_exists <- function(dataset, settings) {
  if (!"dna" %in% names(dataset$tables)) skip_check("no DNA-derived data table")
  occurrenceid_issues(dataset, "dna")
}

occurrenceid_issues <- function(dataset, table_name) {
  if (identical(dataset$core, "occurrence")) {
    skip_check("occurrence core: checked by extension_links_to_core and extension_occurrenceid_matches_coreid")
  }
  data <- require_fields(dataset, table_name, "occurrenceID")
  rows <- which(!is.na(data$occurrenceID) & !data$occurrenceID %chin% occurrence_ids(dataset))
  new_issues(dataset, table_name, rows, field = "occurrenceID", value = data$occurrenceID[rows],
             message = sprintf("The %s's occurrenceID names an occurrence that does not exist in the Occurrence table", record_word(table_name)))
}

# -----------------------------------------------------------------------------
# CHECK  emof_event_matches_occurrence
# What:  for eMoF records with an occurrenceID, the linked event is the event of that occurrence.
# Why:   an occurrence belongs to one event, so its measurements cannot point to another event.
# -----------------------------------------------------------------------------
check_emof_event_matches_occurrence <- function(dataset, settings) {
  if (!identical(dataset$core, "event")) skip_check("the core is not an event table")
  emof_name <- emof_table_name(dataset)
  if (is.na(emof_name)) skip_check("no eMoF table")
  emof <- require_fields(dataset, emof_name, "occurrenceID")
  occurrences <- require_table(dataset, "occurrence")

  occurrence_row <- emof_occurrence_row(dataset)
  event_of_occurrence <- occurrences$.coreid[occurrence_row]
  rows <- which(!is.na(event_of_occurrence) & !is.na(emof$.coreid) & emof$.coreid != event_of_occurrence)
  new_issues(dataset, emof_name, rows, field = "id (core link)", value = emof$.coreid[rows],
             message = "The measurement is linked to another event than its occurrence",
             suggestion = paste("event of the occurrence:", event_of_occurrence[rows]))
}

# -----------------------------------------------------------------------------
# CHECK  emof_record_linked
# What:  every eMoF record is linked to an event and/or an occurrence.
# Why:   a measurement without a link cannot be interpreted.
# -----------------------------------------------------------------------------
check_emof_record_linked <- function(dataset, settings) {
  emof_name <- emof_table_name(dataset)
  if (is.na(emof_name)) skip_check("no eMoF table")
  emof <- dataset$tables[[emof_name]]
  no_link <- is.na(emof$.coreid)
  if ("occurrenceID" %in% names(emof)) no_link <- no_link & is.na(emof$occurrenceID)
  if ("eventID" %in% names(emof)) no_link <- no_link & is.na(emof$eventID)
  new_issues(dataset, emof_name, which(no_link), field = "id / occurrenceID / eventID",
             message = "The measurement is not linked to any event or occurrence")
}

# -----------------------------------------------------------------------------
# CHECK  measurementid_unique
# What:  if measurementID is used, no two measurements share it.
# Why:   measurementID identifies one single measurement.
# -----------------------------------------------------------------------------
check_measurementid_unique <- function(dataset, settings) {
  emof_name <- emof_table_name(dataset)
  if (is.na(emof_name)) skip_check("no eMoF table")
  emof <- require_fields(dataset, emof_name, "measurementID")
  rows <- duplicated_rows(emof$measurementID)
  new_issues(dataset, emof_name, rows, field = "measurementID", value = emof$measurementID[rows],
             message = "measurementID is used by more than one measurement")
}

# -----------------------------------------------------------------------------
# CHECK  eventid_equals_occurrenceid
# What:  reports when (nearly) every event has one single occurrence whose
#        occurrenceID equals the eventID.
# Why:   usually occurrence data forced into an event core; worth a look.
# -----------------------------------------------------------------------------
check_eventid_equals_occurrenceid <- function(dataset, settings) {
  if (!identical(dataset$core, "event")) skip_check("the core is not an event table")
  occurrences <- require_fields(dataset, "occurrence", "occurrenceID")
  same <- !is.na(occurrences$.coreid) & occurrences$occurrenceID == occurrences$.coreid
  one_per_event <- !anyDuplicated(occurrences$.coreid[!is.na(occurrences$.coreid)])
  if (!one_per_event || mean(same, na.rm = TRUE) < 0.9) return(empty_issues())
  new_issues(dataset, "occurrence", which(same), field = "occurrenceID",
             value = sprintf("%s of %s occurrences", format(sum(same), big.mark = ","), format(nrow(occurrences), big.mark = ",")),
             message = "Every event has one occurrence with the same ID: occurrence data in an event core?")
}
