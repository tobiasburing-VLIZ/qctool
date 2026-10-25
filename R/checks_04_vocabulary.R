# =============================================================================
# Checks, group "vocabulary": do fields with a controlled vocabulary use its terms?
#
#   basisofrecord_valid     occurrencestatus_valid   sex_valid
#   lifestage_valid         taxonrank_valid          dctype_valid
#   countrycode_valid       geodeticdatum_valid      establishment_terms_valid
#   license_field_valid
#
# The accepted terms are in inst/reference/vocabularies/<name>.csv:
#   term             the accepted spelling
#   also_written_as  other spellings that get the term as suggestion (| separated)
# Differences in capitals, spaces, '_' and '-' also get a suggestion:
# 'MATERIAL_SAMPLE' -> 'MaterialSample', 'super kingdom' -> 'superkingdom'.
# =============================================================================

# Issues for one field against one vocabulary, in every table that has the field.
vocabulary_issues <- function(dataset, field, vocabulary, message = NULL) {
  tables <- tables_with_fields(dataset, field)
  if (!length(tables)) skip_check(sprintf("no %s column", field))
  terms <- reference_table(file.path("vocabularies", paste0(vocabulary, ".csv")))
  if (is.null(message)) message <- sprintf("Not a term of the %s vocabulary", vocabulary)

  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]][[field]]
    rows <- rows_where(values, function(v) !v %chin% terms$term & !is_placeholder(v))
    if (!length(rows)) return(empty_issues())
    suggestion <- suggest_vocabulary_term(values[rows], terms)
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = ifelse(is.na(suggestion), message, "Written differently than in the vocabulary"),
               suggestion = suggestion)
  }))
}

# The accepted term for other spellings of it, or NA.
suggest_vocabulary_term <- function(values, terms) {
  variants <- strsplit(ifelse(is.na(terms$also_written_as), "", terms$also_written_as), "|", fixed = TRUE)
  other_spellings <- unname(Map(function(term, other) stats_setnames(rep(term, length(other)), simple_name(other)),
                                terms$term, variants))
  lookup <- c(stats_setnames(terms$term, simple_name(terms$term)), unlist(other_spellings))
  lookup <- lookup[nzchar(names(lookup)) & !duplicated(names(lookup))]
  unname(lookup[simple_name(values)])
}

stats_setnames <- function(x, names) {
  names(x) <- names
  x
}

# -----------------------------------------------------------------------------
# CHECK  basisofrecord_valid
# What:  basisOfRecord is a Darwin Core basisOfRecord term.
# Why:   software uses it to know what kind of evidence a record is.
# -----------------------------------------------------------------------------
check_basisofrecord_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "basisOfRecord", "basisOfRecord")
}

# -----------------------------------------------------------------------------
# CHECK  occurrencestatus_valid
# What:  occurrenceStatus is present, absent, detected or notDetected (lower case;
#        detected/notDetected accepted for EMODnet, decided 2026-10-23).
# Why:   one of the four essentials of every record.
# -----------------------------------------------------------------------------
check_occurrencestatus_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "occurrenceStatus", "occurrenceStatus")
}

# -----------------------------------------------------------------------------
# CHECK  sex_valid
# What:  sex is a term of the GBIF Sex vocabulary (female, male, hermaphrodite).
# -----------------------------------------------------------------------------
check_sex_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "sex", "sex")
}

# -----------------------------------------------------------------------------
# CHECK  lifestage_valid
# What:  lifeStage is a term of the life stage list (GBIF + common marine stages).
# -----------------------------------------------------------------------------
check_lifestage_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "lifeStage", "lifeStage")
}

# -----------------------------------------------------------------------------
# CHECK  taxonrank_valid
# What:  taxonRank is a standard rank name (GBIF and WoRMS ranks).
# -----------------------------------------------------------------------------
check_taxonrank_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "taxonRank", "taxonRank")
}

# -----------------------------------------------------------------------------
# CHECK  dctype_valid
# What:  dcterms:type is a DCMI Type term. Kinds of events (cruise, station,
#        sample, ...) belong in eventType.
# Why:   mixing the two meanings makes the field useless for filtering.
# -----------------------------------------------------------------------------
check_dctype_valid <- function(dataset, settings) {
  issues <- vocabulary_issues(dataset, "type", "dcType")
  event_kind <- grepl("(?i)cruise|station|sample|site|transect|haul|trawl|survey|expedition|deployment|campaign|grab|core|dive|tow",
                      issues$problems$value, perl = TRUE)
  issues$problems$message[event_kind] <- "Describes the kind of event; dcterms:type is for DCMI types (Event, PhysicalObject, ...)"
  issues$problems$suggestion[event_kind] <- "move to eventType"
  issues
}

# -----------------------------------------------------------------------------
# CHECK  countrycode_valid
# What:  countryCode is a two-letter ISO 3166-1 code.
# -----------------------------------------------------------------------------
check_countrycode_valid <- function(dataset, settings) {
  vocabulary_issues(dataset, "countryCode", "countryCode", message = "Not a two-letter ISO 3166-1 country code")
}

# -----------------------------------------------------------------------------
# CHECK  geodeticdatum_valid
# What:  geodeticDatum is an EPSG code (EPSG:4326) or a recognised name (WGS84, ...).
# Why:   coordinates can only be placed correctly when the datum is known.
# -----------------------------------------------------------------------------
check_geodeticdatum_valid <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "geodeticDatum")
  if (!length(tables)) skip_check("no geodeticDatum column")
  known_names <- c("wgs84", "nad27", "nad83", "etrs89", "ed50", "gda94", "gda2020", "notrecorded", "unknown")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$geodeticDatum
    rows <- rows_where(values, function(v) {
      !grepl("^EPSG:\\d{4,5}$", trimws(v)) & !simple_name(v) %in% known_names & !is_placeholder(v)
    })
    new_issues(dataset, table_name, rows, field = "geodeticDatum", value = values[rows],
               message = "Not an EPSG code or a recognised datum name",
               suggestion = ifelse(grepl("(?i)wgs", values[rows], perl = TRUE), "EPSG:4326", NA_character_))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  establishment_terms_valid
# What:  establishmentMeans, degreeOfEstablishment and pathway use the GBIF vocabularies.
# Why:   needed to flag introduced and invasive species consistently.
# -----------------------------------------------------------------------------
check_establishment_terms_valid <- function(dataset, settings) {
  fields <- c("establishmentMeans", "degreeOfEstablishment", "pathway")
  present <- fields[vapply(fields, function(f) length(tables_with_fields(dataset, f)) > 0, logical(1))]
  if (!length(present)) skip_check("no establishmentMeans, degreeOfEstablishment or pathway column")
  combine_issues(lapply(present, function(field) vocabulary_issues(dataset, field, field)))
}

# -----------------------------------------------------------------------------
# CHECK  license_field_valid
# What:  if the data has a license column, it is CC0, CC-BY or CC-BY-NC.
# Why:   GBIF only accepts these three; a licence must be machine-readable.
# -----------------------------------------------------------------------------
check_license_field_valid <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "license")
  if (!length(tables)) skip_check("no license column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$license
    rows <- rows_where(values, function(v) license_kind(v) == "other")
    new_issues(dataset, table_name, rows, field = "license", value = values[rows],
               message = "Not one of the accepted licences: CC0, CC-BY or CC-BY-NC (a short name such as 'CC-BY' is enough)",
               suggestion = "CC-BY")
  }))
}

# "CC0", "CC-BY", "CC-BY-NC" or "other", from a licence URL, a name ('CC-BY 4.0',
# 'cc by', 'ccby') or a text. A plain 'CC' counts as CC-BY (the DM team accepts it).
license_kind <- function(text) {
  t <- tolower(trimws(ifelse(is.na(text), "", text)))
  restricted <- grepl("by[-_ ]?nc[-_ ]?sa|by[-_ ]?nc[-_ ]?nd|by[-_ ]?sa|by[-_ ]?nd|share ?alike|no ?deriv", t)
  ifelse(grepl("publicdomain/zero|\\bcc[-_ ]?0\\b|cc[-_ ]?zero|public domain", t), "CC0",
  ifelse(restricted, "other",
  ifelse(grepl("licenses/by-nc/|cc[-_ ]?by[-_ ]?nc|non[- ]?commercial", t), "CC-BY-NC",
  ifelse(grepl("licenses/by/|cc[-_ ]?by\\b|creative commons attribution", t) | grepl("^(cc|creative commons)[-_ ]?([0-9.]+)?$", t), "CC-BY",
         "other"))))
}
