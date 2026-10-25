# =============================================================================
# Checks, group "duplicates"
#
#   duplicate_occurrences          duplicate_emof_records
#   duplicate_type_per_occurrence  duplicate_type_per_event
#
# Fully identical rows are reported by exact_duplicate_rows (structure); these
# checks look at the fields that make a record or measurement what it is.
# =============================================================================

# -----------------------------------------------------------------------------
# CHECK  duplicate_occurrences
# What:  occurrences identical in all chosen fields (inherited event fields and
#        specimen identifiers included), in all their eMoF values, and for DNA
#        data in their sequence. Fields a dataset does not have are skipped.
# Not compared: records of one measured individual (a length or width in the
#        eMoF). Several fish of one species with the same length, sex and stage
#        in one haul are normal (FIFD: 6 million such records), and a copied
#        record keeps its occurrenceID (occurrenceid_unique). A weight alone does
#        not count: it is often the total weight of the species in the sample.
# List:  inst/reference/duplicate_fields.csv
# -----------------------------------------------------------------------------
check_duplicate_occurrences <- function(dataset, settings) {
  occurrences <- require_table(dataset, "occurrence")
  if (!any(c("scientificName", "scientificNameID") %in% names(occurrences))) skip_check("no scientificName or scientificNameID column")
  record_fields <- reference_table("duplicate_fields.csv")$field

  compared <- setdiff(seq_len(nrow(occurrences)), measured_individuals(dataset))
  if (!length(compared)) return(empty_issues())
  used <- record_fields[vapply(record_fields, function(f) field_available(dataset, "occurrence", f), logical(1))]
  key <- as.data.table(lapply(stats_setnames(used, used), function(f) inherited_value(dataset, "occurrence", f)$value[compared]))
  # Records of different events (e.g. replicate samples) are not duplicates.
  if (!"eventID" %in% used && any(!is.na(occurrences$.coreid))) key[, event_link := occurrences$.coreid[compared]]

  # DNA data: different ASVs/OTUs of the same taxon in one sample are not duplicates.
  dna <- dataset$tables$dna
  if (!is.null(dna) && "DNA_sequence" %in% names(dna) && identical(dataset$core, "occurrence")) {
    key[, dna_sequence := dna$DNA_sequence[match(occurrences$.coreid[compared], dna$.coreid)]]
  }

  # All measurements of the occurrence (sex, life stage, ...).
  fingerprint <- emof_fingerprint(dataset)
  for (name in names(fingerprint)) set(key, j = paste0("emof_", name), value = fingerprint[[name]][compared])

  key[, group := .GRP, by = names(key)]
  key[, `:=`(size = .N, first = compared[.I[1]]), by = group]
  duplicates <- which(key$size > 1)
  rows <- compared[duplicates]
  first <- key$first[duplicates]
  first_id <- column_or_na(occurrences, "occurrenceID")[first]
  new_issues(dataset, "occurrence", rows, field = paste(used, collapse = "; "),
             value = sprintf("group of %d, first: %s", key$size[duplicates], fifelse(is.na(first_id), paste("line", occurrences$.line[first]), first_id)),
             message = "Potential duplicate: identical in all compared fields")
}

# Occurrence rows that are one measured individual: they have a length or width in the eMoF.
measured_individuals <- function(dataset) {
  name <- emof_table_name(dataset)
  if (is.na(name)) return(integer())
  occurrence_row <- emof_occurrence_row(dataset)
  individual <- is_size_measurement(dataset$tables[[name]], kinds = c("length", "width"))
  sort(unique(occurrence_row[individual & !is.na(occurrence_row)]))
}

# Two numbers per occurrence that are equal when all its eMoF values (type +
# value) are equal; NULL when there are none. Order-independent, and without
# building text for millions of measurements.
emof_fingerprint <- function(dataset) {
  name <- emof_table_name(dataset)
  if (is.na(name) || !"measurementValue" %in% names(dataset$tables[[name]])) return(NULL)
  emof <- dataset$tables[[name]]
  occurrence_row <- emof_occurrence_row(dataset)
  selected <- which(!is.na(occurrence_row))
  if (!length(selected)) return(NULL)
  type_code <- value_codes(type_key(emof)[selected]) * 1e7
  # The value ID counts too: some datasets name what was measured there
  # (e.g. which fatty acid, northern shrimp dataset).
  pairs <- data.table(row = occurrence_row[selected],
                      value = type_code + value_codes(emof$measurementValue[selected]),
                      value_id = type_code + value_codes(column_or_na(emof, "measurementValueID")[selected]))
  sums <- pairs[, .(f1 = sum(value), f2 = sum(value^2 %% 999983), f3 = sum(value_id), f4 = sum(value_id^2 %% 999983)), by = row]
  n <- nrow(dataset$tables$occurrence)
  fingerprint <- list(f1 = rep(0, n), f2 = rep(0, n), f3 = rep(0, n), f4 = rep(0, n))
  for (name in names(fingerprint)) fingerprint[[name]][sums$row] <- sums[[name]]
  fingerprint
}

# -----------------------------------------------------------------------------
# CHECK  duplicate_emof_records
# What:  two eMoF records with the same link, type, typeID, value, valueID and
#        unit (but differing in some other column, e.g. measurementID or remarks).
#        The valueID counts: some datasets name what was measured there (e.g.
#        which fatty acid, northern shrimp dataset).
# -----------------------------------------------------------------------------
check_duplicate_emof_records <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- dataset$tables[[name]]
  key_fields <- intersect(c(".coreid", "occurrenceID", "measurementType", "measurementTypeID",
                            "measurementValue", "measurementValueID", "measurementUnit", "measurementUnitID"), names(data))
  same_measurement <- duplicated(data, by = key_fields)
  identical_row <- duplicated(data, by = setdiff(names(data), ".line"))
  rows <- which(same_measurement & !identical_row)
  new_issues(dataset, name, rows, field = paste(setdiff(key_fields, ".coreid"), collapse = "; "),
             value = column_or_na(data, "measurementType")[rows],
             message = "The same measurement (link, type, value, unit) is recorded more than once")
}

# -----------------------------------------------------------------------------
# CHECK  duplicate_type_per_occurrence / duplicate_type_per_event
# What:  one occurrence (or one event, for measurements without occurrenceID)
#        has the same measurementType or measurementTypeID more than once, with
#        different values.
# How:   the occurrence of a measurement is its occurrenceID, or in an
#        occurrence core (where the eMoF often has no occurrenceID column) the
#        occurrence it is linked to through the id column.
# -----------------------------------------------------------------------------
check_duplicate_type_per_occurrence <- function(dataset, settings) {
  name <- require_emof(dataset)
  occurrence <- emof_occurrence_of(dataset, name)
  if (all(is.na(occurrence))) skip_check(sprintf("no occurrenceID column in the %s table, and the core is not an occurrence table", name))
  same_type_issues(dataset, name, which(!is.na(occurrence)), occurrence, "occurrence")
}

check_duplicate_type_per_event <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- dataset$tables[[name]]
  occurrence <- emof_occurrence_of(dataset, name)
  event_link <- if (identical(dataset$core, "event")) data$.coreid else column_or_na(data, "eventID")
  same_type_issues(dataset, name, which(is.na(occurrence) & !is.na(event_link)), event_link, "event")
}

# The occurrence each eMoF record belongs to: its occurrenceID, or in an
# occurrence core its link to the core (id column). NA: an event measurement.
emof_occurrence_of <- function(dataset, name) {
  data <- dataset$tables[[name]]
  occurrence <- column_or_na(data, "occurrenceID")
  if (identical(dataset$core, "occurrence")) occurrence <- fifelse(is.na(occurrence), data$.coreid, occurrence)
  occurrence
}

same_type_issues <- function(dataset, name, candidates, owner, owner_label) {
  data <- dataset$tables[[name]]
  if (!length(candidates)) return(empty_issues())
  owner_code <- value_codes(owner[candidates])
  combine_issues(lapply(intersect(c("measurementType", "measurementTypeID"), names(data)), function(field) {
    type <- data[[field]][candidates]
    # Fast for tens of millions of rows: one number per (owner, type) pair, and
    # the values are only compared for pairs that occur more than once.
    type_code <- value_codes(type)
    pair <- owner_code * (max(type_code) + 1) + type_code
    repeated <- which(!is.na(type) & (duplicated(pair) | duplicated(pair, fromLast = TRUE)))
    if (!length(repeated)) return(empty_issues())
    measurements <- data.table(row = candidates[repeated], pair = pair[repeated],
                               value = data$measurementValue[candidates[repeated]])
    differing <- measurements[, .(values = uniqueN(value)), by = pair][values > 1]
    rows <- measurements[pair %in% differing$pair, row]
    new_issues(dataset, name, rows, field = field, value = data[[field]][rows],
               message = sprintf("The same %s occurs more than once for one %s, with different values", field, owner_label))
  }))
}
