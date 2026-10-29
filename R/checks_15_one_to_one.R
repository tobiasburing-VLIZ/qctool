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
# A value with several IDs, or an ID with several values: a broken one-to-one
# relationship, an error both ways (the severity is in the rules workbook).
# Empty values are left out. Like BioCheck's one_to_one_check().
#
# Each value with several IDs is one problem, which names all its IDs with
# their number of records, so the conflict is seen at once:
#   value 'metres', suggestion "use one measurementUnitID: 123A (40 records) or 123B (3 records)"
# Its records are those with a less common ID (here the 3 with 123B). The
# field of the problem names the value's field first: "measurementUnit;
# measurementUnitID" for a unit with several IDs, "measurementUnitID;
# measurementUnit" for an ID with several units.
#
# Every combination of such a conflict (metres -> 123A, metres -> 123B) is
# also kept with the dataset (remember_pair()), for qc_one_to_one(), the
# Overview and screen 3, which show them next to each other.
# =============================================================================

# Records whose value and ID do not pair up one-to-one, as issues.
#   within      one value per row to compare inside, e.g. the measurement type
#               (a valueID only needs one value within its type)
#   both_ways   FALSE: only "one ID with several values" (e.g. organismID -> scientificName,
#               where one taxon has many organisms)
#   only        TRUE for the rows to compare (default all)
#   within_text a function(rows) that names the 'within' group of these rows,
#               e.g. "measurementType 'life stage'" (only called for groups
#               with a conflict: quick on big tables)
one_to_one_issues <- function(dataset, table_name, value_field, id_field, within = NULL, both_ways = TRUE, only = NULL,
                              within_text = NULL) {
  data <- dataset$tables[[table_name]]
  value <- data[[value_field]]
  id <- data[[id_field]]
  if (is.null(value) || is.null(id)) return(empty_issues())
  remember_pair(dataset, table_name, value_field, id_field, NULL, both_ways = both_ways)   # compared; nothing found (yet)
  keep <- !is_empty_value(value) & !is_empty_value(id)
  if (!is.null(only)) keep <- keep & only
  filled <- which(keep)
  if (!length(filled)) return(empty_issues())
  # Numbers instead of texts: quick also for millions of rows. Code k stands for the k-th of the levels.
  value_levels <- unique(value[filled])
  id_levels <- unique(id[filled])
  pairs <- data.table(row = filled, v = value_codes(value[filled]), i = value_codes(id[filled]),
                      w = if (is.null(within)) 0L else value_codes(within[filled]))
  counts <- pairs[, .(N = .N, first_row = row[1L]), by = .(w, v, i)]

  # Per value (or ID: 'key') with more than one partner: all its combinations,
  # the most used first, with the partners in words.
  conflicts <- function(key, partner, partner_levels) {
    group <- c("w", key)
    several <- counts[counts[, .(kinds = .N), by = group][kinds > 1], on = group]
    if (!nrow(several)) return(NULL)
    setorderv(several, c(group, "N"), order = c(1L, 1L, -1L))
    several[, `:=`(usual = seq_len(.N) == 1L, partners = partner_list(partner_levels[get(partner)], N),
                   within_group = if (is.null(within_text)) NA_character_ else within_text(first_row[1L])), by = group]
    several
  }
  several_ids <- if (both_ways) conflicts("v", "i", id_levels)
  several_values <- conflicts("i", "v", value_levels)

  # The combinations, for the tables that show them next to each other.
  combinations <- function(several, key_field, partner_field, keys, partners) {
    if (is.null(several)) return(NULL)
    key_values <- keys[several[[if (key_field == value_field) "v" else "i"]]]
    data.table(conflict = sprintf("%s%s '%s' has %d different %ss",
                                  ifelse(is.na(several$within_group), "", sprintf("within %s: ", several$within_group)),
                                  key_field, key_values, several$kinds, partner_field),
               # (not 'key = ...': in data.table() that sets a sort key)
               field = key_field, value = key_values,
               other_field = partner_field, other_value = partners[several[[if (key_field == value_field) "i" else "v"]]],
               records = several$N, less_common = !several$usual, within_group = several$within_group)
  }
  remember_pair(dataset, table_name, value_field, id_field, both_ways = both_ways, rbind(
    combinations(several_ids, value_field, id_field, value_levels, id_levels),
    combinations(several_values, id_field, value_field, id_levels, value_levels)))

  # The issues: the records of the less common combinations.
  issues_of <- function(several, key, partner, key_field, partner_field, keys) {
    if (is.null(several)) return(NULL)
    odd <- pairs[several[usual == FALSE], on = c("w", key, partner), nomatch = 0L]
    prefix <- ifelse(is.na(odd$within_group), "", sprintf("within %s: ", odd$within_group))
    new_issues(dataset, table_name, odd$row, field = paste(key_field, partner_field, sep = "; "), value = keys[odd[[key]]],
               message = if (key_field == value_field) {
                 sprintf("The same %s has several %ss in this dataset; this record uses a less common one", key_field, partner_field)
               } else {
                 sprintf("The same %s goes with several %ss in this dataset; this record uses a less common one", key_field, partner_field)
               },
               suggestion = sprintf("%suse one %s: %s", prefix, partner_field, odd$partners))
  }
  combine_issues(list(issues_of(several_ids, "v", "i", value_field, id_field, value_levels),
                      issues_of(several_values, "i", "v", id_field, value_field, id_levels)))
}

# What a one-to-one comparison found, kept with the dataset (dataset$cache) for
# qc_one_to_one(), the Overview and screen 3: which pair was compared in which
# table, and every combination of each conflict. add = TRUE adds combinations.
remember_pair <- function(dataset, table_name, value_field, id_field, combinations, add = FALSE, both_ways = TRUE) {
  if (is.null(dataset$cache)) return(invisible())
  key <- paste("one_to_one", table_name, value_field, id_field, sep = "|")
  if (add && exists(key, envir = dataset$cache, inherits = FALSE)) {
    earlier <- get(key, envir = dataset$cache)
    combinations <- rbind(earlier$combinations, combinations)
    both_ways <- earlier$both_ways
  }
  assign(key, list(table = table_name, value_field = value_field, id_field = id_field, both_ways = both_ways,
                   combinations = combinations), envir = dataset$cache)
  invisible()
}

# All that remember_pair() kept: one list per pair and table.
remembered_pairs <- function(dataset) {
  if (is.null(dataset$cache)) return(list())
  keys <- sort(grep("^one_to_one\\|", ls(dataset$cache), value = TRUE))
  lapply(keys, get, envir = dataset$cache)
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
  own_id <- c(event = "eventID", occurrence = "occurrenceID")   # unique in their own table: nothing to compare there
  found <- list()
  for (p in seq_len(nrow(pairs))) {
    for (table_name in tables_with_both(dataset, pairs$id_field[p], pairs$value_field[p])) {
      if (identical(unname(own_id[table_name]), pairs$id_field[p])) next
      found[[length(found) + 1]] <- one_to_one_issues(dataset, table_name, pairs$value_field[p], pairs$id_field[p],
                                                      both_ways = pairs$direction[p] == "both ways")
    }
  }
  if (!length(found)) skip_check("no table has both fields of any pair in reference/one_to_one_pairs.csv")
  combine_issues(found)
}
