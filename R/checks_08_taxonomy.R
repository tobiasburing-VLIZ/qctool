# =============================================================================
# Checks, group "taxonomy"
#
#   offline:  scientificnameid_worms_format    scientificname_clean
#   WoRMS:    scientificnameid_exists_in_worms aphiaid_accepted
#             scientificname_matches_aphiaid   taxon_identified_to_family_or_lower
#             taxon_is_marine                  names_without_id_matched
#
# WoRMS lookups (each AphiaID and name once, cached): R/31_lookup_worms.R.
# Without internet these checks are "not performed", unless the cache knows
# all IDs already. marine_taxon_on_land is in checks_06_location.R.
# =============================================================================

worms_lsid_pattern <- "^urn:lsid:marinespecies\\.org:taxname:\\d+$"

# -----------------------------------------------------------------------------
# CHECK  scientificnameid_worms_format
# What:  scientificNameID has the form urn:lsid:marinespecies.org:taxname:<number>.
# Why:   EMODnet and OBIS use WoRMS IDs to know exactly which taxon is meant.
# -----------------------------------------------------------------------------
check_scientificnameid_worms_format <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "scientificNameID")
  values <- occurrences$scientificNameID
  rows <- rows_where(values, function(v) !grepl(worms_lsid_pattern, v) & !is_placeholder(v))
  new_issues(dataset, "occurrence", rows, field = "scientificNameID", value = values[rows],
             message = "Not a WoRMS LSID (urn:lsid:marinespecies.org:taxname:<AphiaID>)",
             suggestion = suggest_worms_lsid(values[rows]))
}

# "117081", "https://www.marinespecies.org/aphia.php?p=taxdetails&id=117081",
# " urn:lsid:marinespecies.org:taxname:117081" -> "urn:lsid:marinespecies.org:taxname:117081"
suggest_worms_lsid <- function(x) {
  aphia_id <- rep(NA_character_, length(x))
  v <- trimws(x)
  only_number <- grepl("^\\d+$", v)
  aphia_id[only_number] <- v[only_number]
  from_url <- !only_number & grepl("marinespecies\\.org.*[?&]id=(\\d+)", v)
  aphia_id[from_url] <- sub(".*[?&]id=(\\d+).*", "\\1", v[from_url])
  from_lsid <- !only_number & !from_url & grepl("(?i)marinespecies\\.org:taxname:\\s*\\d+", v, perl = TRUE)
  aphia_id[from_lsid] <- sub("(?i).*taxname:\\s*(\\d+).*", "\\1", v[from_lsid], perl = TRUE)
  ifelse(is.na(aphia_id), NA_character_, paste0("urn:lsid:marinespecies.org:taxname:", aphia_id))
}

# -----------------------------------------------------------------------------
# CHECK  scientificname_clean
# What:  scientificName holds only the name: no qualifiers (sp., cf., aff., ?),
#        no authorship, no underscores, no 'uncultured' / 'environmental sample'.
# Why:   extra words prevent matching the name to WoRMS.
# List:  inst/reference/name_patterns.csv (pattern, problem, where it should go)
# -----------------------------------------------------------------------------
check_scientificname_clean <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "scientificName")
  patterns <- reference_table("name_patterns.csv")
  values <- occurrences$scientificName

  found <- find_problem_values(values, function(v) {
    problem <- rep(NA_character_, length(v))
    for (i in seq_len(nrow(patterns))) {
      todo <- is.na(problem) & validUTF8(v)
      hit <- todo & grepl(patterns$pattern[i], v, perl = TRUE)
      problem[hit] <- paste0(i, "|", patterns$problem[i])
    }
    problem
  })
  if (!nrow(found)) return(empty_issues())

  pattern_number <- as.integer(sub("\\|.*$", "", found$message))
  names <- values[found$row]
  cleaned <- trimws(gsub("\\s{2,}", " ", mapply(function(name, n) {
    if (patterns$move_to[n] == "replace by spaces") gsub("_", " ", name) else sub(patterns$pattern[n], " ", name, perl = TRUE)
  }, names, pattern_number, USE.NAMES = FALSE)))
  move_to <- patterns$move_to[pattern_number]
  not_a_taxon <- grepl("not a taxon", patterns$problem[pattern_number])
  only_cleaning <- grepl("^(replace|remove)", move_to)       # underscores, double spaces
  new_issues(dataset, "occurrence", found$row, field = "scientificName", value = names,
             message = paste("scientificName", sub("^\\d+\\|", "", found$message)),
             suggestion = ifelse(not_a_taxon, move_to,
                          ifelse(only_cleaning, cleaned, sprintf("'%s' (the rest goes to %s)", cleaned, move_to))))
}

# --- WoRMS -------------------------------------------------------------------------

# The AphiaID of every occurrence (from scientificNameID, NA when there is
# none) and the WoRMS record of each different AphiaID.
occurrence_worms <- function(dataset) {
  occurrences <- require_fields(dataset, "occurrence", "scientificNameID")
  ids <- cached(dataset, "occurrence_aphia_ids", aphia_id(occurrences$scientificNameID))
  if (all(is.na(ids))) skip_check("no WoRMS AphiaID in scientificNameID")
  list(ids = ids, records = worms_records(dataset, ids))
}

# The rows whose AphiaID is in the table 'selected', and the record for each of those rows.
rows_of_taxa <- function(ids, selected) {
  rows <- which(ids %chin% selected$query)
  list(rows = rows, record = selected[match(ids[rows], selected$query)])
}

# -----------------------------------------------------------------------------
# CHECK  scientificnameid_exists_in_worms
# What:  the AphiaID in scientificNameID exists in WoRMS.
# Suggestion: the WoRMS match for the scientificName of the record, if any.
# -----------------------------------------------------------------------------
check_scientificnameid_exists_in_worms <- function(dataset, settings) {
  taxa <- occurrence_worms(dataset)
  unknown <- taxa$records[found == FALSE]
  found <- rows_of_taxa(taxa$ids, unknown)
  if (!length(found$rows)) return(empty_issues())
  occurrences <- dataset$tables$occurrence
  names <- column_or_na(occurrences, "scientificName")[found$rows]
  new_issues(dataset, "occurrence", found$rows, field = "scientificNameID", value = occurrences$scientificNameID[found$rows],
             message = "This AphiaID does not exist in WoRMS",
             suggestion = name_suggestions(dataset, names))
}

# "urn:lsid:...:141433 (Abra alba, exact match)" for names WoRMS knows; NA
# otherwise, also when WoRMS cannot be reached (a suggestion is a bonus, not a
# reason to fail the check).
name_suggestions <- function(dataset, names) {
  wanted <- !is_empty_value(names)
  if (!any(wanted)) return(rep(NA_character_, length(names)))
  matches <- tryCatch(worms_name_matches(dataset, unique(names[wanted])), qc_not_performed = function(condition) NULL)
  if (is.null(matches)) return(rep(NA_character_, length(names)))
  m <- matches[match(names, matches$query)]
  fifelse(m$found %in% TRUE & m$n_matches %in% 1L, sprintf("%s (%s, %s match)", lsid_of(m$id), m$scientificname, m$match_type), NA_character_)
}

# -----------------------------------------------------------------------------
# CHECK  aphiaid_accepted
# What:  unaccepted, deleted or quarantined AphiaIDs (or IDs for which WoRMS
#        names another valid taxon), with the accepted taxon as suggestion.
# -----------------------------------------------------------------------------
check_aphiaid_accepted <- function(dataset, settings) {
  taxa <- occurrence_worms(dataset)
  not_accepted <- taxa$records[found == TRUE & (status %chin% c("unaccepted", "deleted", "quarantined") |
                                                  (!is.na(valid_id) & valid_id != query))]
  found <- rows_of_taxa(taxa$ids, not_accepted)
  record <- found$record
  new_issues(dataset, "occurrence", found$rows, field = "scientificNameID",
             value = sprintf("%s (%s)", record$scientificname, record$status),
             message = "The AphiaID is not the accepted taxon in WoRMS",
             suggestion = fifelse(is.na(record$valid_id), "WoRMS gives no accepted taxon",
                                  sprintf("%s (%s)", lsid_of(record$valid_id), record$valid_name)))
}

# -----------------------------------------------------------------------------
# CHECK  scientificname_matches_aphiaid
# What:  scientificName is the WoRMS name of the AphiaID. Authorship after the
#        name is ignored here (scientificname_clean reports it). Up to
#        max_distance different characters: warning; more: error.
# Settings: max_distance (5)
# -----------------------------------------------------------------------------
check_scientificname_matches_aphiaid <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", c("scientificName", "scientificNameID"))
  taxa <- occurrence_worms(dataset)
  max_distance <- setting(settings, "max_distance", 5)

  # Each different (name, AphiaID) pair once.
  names <- occurrences$scientificName
  pair <- value_codes(names) * (length(names) + 1) + value_codes(taxa$ids)
  first <- which(!duplicated(pair) & !is.na(taxa$ids) & !is_empty_value(names))
  record <- taxa$records[match(taxa$ids[first], taxa$records$query)]
  pairs <- data.table(pair = pair[first], name = names[first], worms_name = record$scientificname)[!is.na(worms_name)]
  if (!nrow(pairs)) return(empty_issues())
  pairs[, distance := name_distance(name, worms_name)]
  wrong <- pairs[distance > 0]
  if (!nrow(wrong)) return(empty_issues())

  rows <- which(pair %in% wrong$pair)
  index <- match(pair[rows], wrong$pair)   # outside wrong[...]: there 'pair' would mean its column
  w <- wrong[index]
  close <- w$distance <= max_distance
  new_issues(dataset, "occurrence", rows, field = "scientificName", value = w$name,
             message = ifelse(close, sprintf("scientificName differs slightly from the WoRMS name of this AphiaID (%d characters)", w$distance),
                              "scientificName is not the WoRMS name of this AphiaID"),
             suggestion = w$worms_name,
             severity_hint = ifelse(close, "warning", NA_character_))
}

# Number of characters that differ; 0 when the name is the WoRMS name, also
# when only authorship follows ("Abra alba (W. Wood, 1802)"). Qualifiers
# (sp., spp., cf., aff.) are left out: scientificname_clean reports them.
name_distance <- function(given, worms_name) {
  given <- gsub("\\s+", " ", trimws(given))
  given <- trimws(gsub("(^| )(cf|aff|sp|spp|indet)\\.?(?= |$)", " ", given, perl = TRUE))
  given <- gsub("\\s+", " ", given)
  rest <- substring(given, nchar(worms_name) + 1)
  same <- given == worms_name | (startsWith(given, paste0(worms_name, " ")) & grepl("^ [(A-Z]", rest))
  distance <- mapply(function(a, b) utils::adist(a, b)[1, 1], given, worms_name, USE.NAMES = FALSE)
  as.integer(ifelse(same, 0L, distance))
}

# -----------------------------------------------------------------------------
# CHECK  taxon_identified_to_family_or_lower
# What:  records identified only above family level (order, class, phylum, ...).
# How:   WoRMS rank ID below the one of the chosen rank (140 = family).
# Settings: lowest_rank (family): kingdom, phylum, class, order, family, genus or species
# -----------------------------------------------------------------------------
check_taxon_identified_to_family_or_lower <- function(dataset, settings) {
  lowest_rank <- tolower(setting(settings, "lowest_rank", "family"))
  if (!lowest_rank %in% names(worms_rank_ids)) {
    stop("setting lowest_rank must be one of: ", paste(names(worms_rank_ids), collapse = ", "))
  }
  wanted_rank_id <- worms_rank_ids[[lowest_rank]]   # not 'rank_id': inside records[...] that is the column
  taxa <- occurrence_worms(dataset)
  too_high <- taxa$records[found == TRUE & !is.na(rank_id) & rank_id < wanted_rank_id]
  found <- rows_of_taxa(taxa$ids, too_high)
  new_issues(dataset, "occurrence", found$rows, field = "scientificNameID",
             value = sprintf("%s (%s)", found$record$scientificname, found$record$rank),
             message = sprintf("Identified only above %s level", lowest_rank))
}

# WoRMS rank IDs of the main ranks.
worms_rank_ids <- c(kingdom = 10, phylum = 30, class = 60, order = 100, family = 140, genus = 180, species = 220)

# -----------------------------------------------------------------------------
# CHECK  taxon_is_marine
# What:  taxa that WoRMS lists as freshwater-only or terrestrial-only (not
#        marine, not brackish). Unaccepted names without habitat use the
#        habitat of the accepted taxon.
# -----------------------------------------------------------------------------
check_taxon_is_marine <- function(dataset, settings) {
  taxa <- occurrence_worms(dataset)
  habitat <- taxon_habitat(dataset, taxa$records)
  not_marine <- habitat[!(is_marine %in% 1) & !(is_brackish %in% 1) & (is_freshwater %in% 1 | is_terrestrial %in% 1)]
  found <- rows_of_taxa(taxa$ids, not_marine)
  record <- found$record
  where <- ifelse(record$is_freshwater %in% 1 & record$is_terrestrial %in% 1, "freshwater, terrestrial",
                  ifelse(record$is_freshwater %in% 1, "freshwater", "terrestrial"))
  new_issues(dataset, "occurrence", found$rows, field = "scientificNameID",
             value = sprintf("%s (%s)", record$scientificname, where),
             message = "WoRMS does not list this taxon as marine or brackish")
}

# The habitat flags per AphiaID; for unaccepted IDs without flags, those of the accepted taxon.
taxon_habitat <- function(dataset, records) {
  records <- copy(records[found == TRUE])
  no_flags <- is.na(records$is_marine) & is.na(records$is_brackish) & is.na(records$is_freshwater) & is.na(records$is_terrestrial)
  borrow <- which(no_flags & !is.na(records$valid_id) & records$valid_id != records$query)
  if (length(borrow)) {
    accepted <- worms_records(dataset, records$valid_id[borrow])
    from <- accepted[match(records$valid_id[borrow], accepted$query)]
    for (flag in c("is_marine", "is_brackish", "is_freshwater", "is_terrestrial")) set(records, borrow, flag, from[[flag]])
  }
  records
}

# The taxa that live only in the sea: marine, not brackish, freshwater or terrestrial.
marine_only_taxa <- function(dataset, records) {
  taxon_habitat(dataset, records)[is_marine %in% 1 & !(is_brackish %in% 1) & !(is_freshwater %in% 1) & !(is_terrestrial %in% 1)]
}

# -----------------------------------------------------------------------------
# CHECK  names_without_id_matched
# What:  records with a scientificName but no scientificNameID: the name is
#        looked up in WoRMS and the match is given as suggestion.
# -----------------------------------------------------------------------------
check_names_without_id_matched <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "scientificName")
  names <- per_value(occurrences$scientificName, function(v) gsub("\\s+", " ", trimws(v)))
  without_id <- !is_empty_value(names) & is_empty_value(column_or_na(occurrences, "scientificNameID"))
  if (!any(without_id)) return(empty_issues())

  matches <- worms_name_matches(dataset, names[without_id])
  rows <- which(without_id)
  m <- matches[match(names[rows], matches$query)]
  accepted_note <- fifelse(!is.na(m$status) & m$status != "accepted" & !is.na(m$valid_name), paste0(", accepted: ", m$valid_name), "")
  new_issues(dataset, "occurrence", rows, field = "scientificNameID", value = names[rows],
             message = fcase(!m$found, "Name without scientificNameID, not found in WoRMS",
                             m$n_matches > 1, "Name without scientificNameID: WoRMS has several matches",
                             m$match_type %chin% "exact", "Name without scientificNameID: found in WoRMS",
                             default = "Name without scientificNameID: WoRMS has a similar name"),
             suggestion = fcase(!m$found, NA_character_,
                                m$n_matches > 1, m$all_names,
                                default = sprintf("%s (%s, %s match%s)", lsid_of(m$id), m$scientificname, m$match_type, accepted_note)))
}
