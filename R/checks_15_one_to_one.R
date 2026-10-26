# =============================================================================
# Checks, values and their IDs: does every value go with one ID, and every ID
# with one value, in the whole dataset?
#
#   scientificname_id_one_to_one    scientificName   <-> scientificNameID
#   measurementtype_id_one_to_one   measurementType  <-> measurementTypeID
#   measurementunit_id_one_to_one   measurementUnit  <-> measurementUnitID
#   values_and_ids_one_to_one       the pairs of reference/one_to_one_pairs.csv
#                                   (institutionCode <-> institutionID, country <->
#                                   countryCode, organismID -> scientificName, ...)
#   (measurementValue <-> measurementValueID, per measurement type:
#    measurementvalueid_consistent in checks_09_emof.R)
#
# One value with several IDs: one of the IDs is probably wrong (error).
# One ID with several values: usually spelling, e.g. with and without the
# author (warning). Empty values are left out. Like BioCheck's one_to_one_check().
#
# Each value with several IDs is one problem, which names all its IDs with
# their number of records, so the conflict is seen at once:
#   value 'metres', suggestion "use one measurementUnitID: 123A (40 records) or 123B (3 records)"
# Its records are those with a less common ID (here the 3 with 123B).
# =============================================================================

# Records whose value and ID do not pair up one-to-one, as issues.
#   within      one value per row to compare inside, e.g. the measurement type
#               (a valueID only needs one value within its type)
#   both_ways   FALSE: only "one ID with several values" (e.g. organismID -> scientificName,
#               where one taxon has many organisms)
#   only        TRUE for the rows to compare (default all)
#   within_text a function(rows) that names the 'within' group of these rows in
#               the suggestion, e.g. "measurementType 'life stage'" (only called
#               for rows that are reported: quick on big tables)
one_to_one_issues <- function(dataset, table_name, value_field, id_field, within = NULL, both_ways = TRUE, only = NULL,
                              within_text = NULL) {
  data <- dataset$tables[[table_name]]
  value <- data[[value_field]]
  id <- data[[id_field]]
  if (is.null(value) || is.null(id)) return(empty_issues())
  keep <- !is_empty_value(value) & !is_empty_value(id)
  if (!is.null(only)) keep <- keep & only
  filled <- which(keep)
  if (!length(filled)) return(empty_issues())
  # Numbers instead of texts: quick also for millions of rows. Code k stands for the k-th of the levels.
  value_levels <- unique(value[filled])
  id_levels <- unique(id[filled])
  pairs <- data.table(row = filled, v = value_codes(value[filled]), i = value_codes(id[filled]),
                      w = if (is.null(within)) 0L else value_codes(within[filled]))
  counts <- pairs[, .N, by = .(w, v, i)]
  field <- paste(value_field, id_field, sep = "; ")

  # Per value (or ID: 'key') with more than one partner: all partners in words,
  # the most used first, and the records that use one of the others.
  conflicts <- function(key, partner, partner_levels) {
    group <- c("w", key)
    several <- counts[counts[, .(kinds = .N), by = group][kinds > 1], on = group]
    if (!nrow(several)) return(NULL)
    setorderv(several, c(group, "N"), order = c(1L, 1L, -1L))
    several[, `:=`(usual = seq_len(.N) == 1L, partners = partner_list(partner_levels[get(partner)], N)), by = group]
    odd <- pairs[several[usual == FALSE], on = c(group, partner), nomatch = 0L]
    # "within measurementType 'life stage': ", the same for the whole group
    odd[, prefix := if (is.null(within_text)) "" else sprintf("within %s: ", within_text(row[1])), by = w]
    odd
  }

  issues <- list()
  several_ids <- if (both_ways) conflicts("v", "i", id_levels)
  if (!is.null(several_ids)) {
    issues$ids <- new_issues(dataset, table_name, several_ids$row, field = field, value = value_levels[several_ids$v],
      message = sprintf("The same %s has several %ss in this dataset; this record uses a less common one", value_field, id_field),
      suggestion = sprintf("%suse one %s: %s", several_ids$prefix, id_field, several_ids$partners))
  }
  several_values <- conflicts("i", "v", value_levels)
  if (!is.null(several_values)) {
    issues$values <- new_issues(dataset, table_name, several_values$row, field = field, value = id_levels[several_values$i],
      message = sprintf("The same %s goes with several %ss in this dataset; this record uses a less common one", id_field, value_field),
      suggestion = sprintf("%suse one %s: %s", several_values$prefix, value_field, several_values$partners),
      severity_hint = "warning")
  }
  combine_issues(issues)
}

# The measurement type of each eMoF row in words, for messages: "measurementType 'life stage'"
# (or its measurementTypeID when the text is empty).
type_text <- function(data) {
  type <- column_or_na(data, "measurementType")
  type_id <- column_or_na(data, "measurementTypeID")
  fifelse(is_empty_value(type), sprintf("measurementTypeID '%s'", type_id), sprintf("measurementType '%s'", type))
}

# The partners of one value, with their records, the most used first:
# "123A (40 records) or 123B (3 records)"; more than five: "A (9 records), ... and 4 more".
partner_list <- function(texts, records, max_shown = 5) {
  shown <- seq_len(min(length(texts), max_shown))
  words <- sprintf("%s (%s %s)", shorten(texts[shown], 100), prettyNum(records[shown], big.mark = ","),
                   ifelse(records[shown] == 1, "record", "records"))
  more <- length(texts) - length(shown)
  if (more > 0) return(sprintf("%s and %s more", paste(words, collapse = ", "), prettyNum(more, big.mark = ",")))
  if (length(words) == 1) return(words)
  paste(paste(words[-length(words)], collapse = ", "), "or", words[length(words)])
}

# The tables of a dataset that have both fields.
tables_with_both <- function(dataset, a, b) {
  names(dataset$tables)[vapply(dataset$tables, function(t) all(c(a, b) %in% names(t)), logical(1))]
}

# Records where one of the two fields is empty, while other records with the
# same other field have it filled: the most used partner is the suggestion.
one_side_empty_issues <- function(dataset, table_name, empty_field, filled_field) {
  data <- dataset$tables[[table_name]]
  empty <- is_empty_value(data[[empty_field]])
  filled <- !is_empty_value(data[[filled_field]])
  both <- which(!empty & filled)
  if (!length(both)) return(empty_issues())
  known <- data.table(key_value = data[[filled_field]][both], partner = data[[empty_field]][both])
  known <- known[, .N, by = .(key_value, partner)][order(-N)][!duplicated(key_value)]
  rows <- which(empty & filled)
  rows <- rows[data[[filled_field]][rows] %chin% known$key_value]
  new_issues(dataset, table_name, rows, field = empty_field, value = data[[filled_field]][rows],
             message = sprintf("%s is empty, while other records with this %s have one", empty_field, filled_field),
             suggestion = known$partner[match(data[[filled_field]][rows], known$key_value)], severity_hint = "warning")
}

# -----------------------------------------------------------------------------
# CHECK  scientificname_id_one_to_one
# What:  each scientificName has one scientificNameID and each scientificNameID
#        one scientificName. A record without an ID (or without a name) gets the
#        ID (or name) of the other records as suggestion.
# Why:   one name with two IDs means one ID is wrong; one ID with two names is
#        usually spelling or an author added in some records.
# -----------------------------------------------------------------------------
check_scientificname_id_one_to_one <- function(dataset, settings) {
  tables <- tables_with_both(dataset, "scientificName", "scientificNameID")
  if (!length(tables)) skip_check("no table with both scientificName and scientificNameID")
  combine_issues(lapply(tables, function(table_name) {
    combine_issues(list(
      one_to_one_issues(dataset, table_name, "scientificName", "scientificNameID"),
      one_side_empty_issues(dataset, table_name, "scientificNameID", "scientificName"),
      one_side_empty_issues(dataset, table_name, "scientificName", "scientificNameID")))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  measurementtype_id_one_to_one
# What:  each measurementType goes with one measurementTypeID, and each
#        measurementTypeID with one measurementType text.
# -----------------------------------------------------------------------------
check_measurementtype_id_one_to_one <- function(dataset, settings) {
  name <- require_emof(dataset)
  require_fields(dataset, name, c("measurementType", "measurementTypeID"))
  one_to_one_issues(dataset, name, "measurementType", "measurementTypeID")
}

# -----------------------------------------------------------------------------
# CHECK  measurementunit_id_one_to_one
# What:  each measurementUnit goes with one measurementUnitID, and each
#        measurementUnitID with one measurementUnit text.
# -----------------------------------------------------------------------------
check_measurementunit_id_one_to_one <- function(dataset, settings) {
  name <- require_emof(dataset)
  require_fields(dataset, name, c("measurementUnit", "measurementUnitID"))
  one_to_one_issues(dataset, name, "measurementUnit", "measurementUnitID")
}

# -----------------------------------------------------------------------------
# CHECK  values_and_ids_one_to_one
# What:  the pairs of fields in reference/one_to_one_pairs.csv go one-to-one,
#        in every table that has both: institutionCode and institutionID,
#        recordedBy and recordedByID, country and countryCode, ... Pairs marked
#        'one way' only need one value per ID (an organism is one taxon, but a
#        taxon has many organisms).
# List:  inst/reference/one_to_one_pairs.csv (the DM team can add pairs)
# -----------------------------------------------------------------------------
check_values_and_ids_one_to_one <- function(dataset, settings) {
  pairs <- reference_table("one_to_one_pairs.csv")
  found <- list()
  for (p in seq_len(nrow(pairs))) {
    for (table_name in tables_with_both(dataset, pairs$id_field[p], pairs$value_field[p])) {
      found[[length(found) + 1]] <- one_to_one_issues(dataset, table_name, pairs$value_field[p], pairs$id_field[p],
                                                      both_ways = pairs$direction[p] == "both ways")
    }
  }
  if (!length(found)) skip_check("no table has both fields of any pair in reference/one_to_one_pairs.csv")
  combine_issues(found)
}
