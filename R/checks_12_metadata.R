# =============================================================================
# Checks, group "metadata": the EML, including a basic FAIR check
#
#   eml_present                   eml_title_filled             eml_abstract_filled
#   eml_contact_filled            eml_creator_filled           eml_metadata_provider_filled
#   eml_license_valid             eml_citation_filled          eml_geographic_coverage_filled
#   eml_temporal_coverage_filled  eml_taxonomic_coverage_filled eml_keywords_filled
#   eml_persistent_identifier     eml_people_have_orcid        eml_organisation_identifier
#   eml_methods_described         eml_title_informative        eml_abstract_informative
#   eml_language_given            eml_no_placeholders          ipt_url_length
#   license_data_matches_eml
#
# The EML is read into dataset$metadata by R/13_read_eml.R. Issues point to
# the metadata element (field), in the file eml.xml.
# =============================================================================

# The metadata, or skip the check (simple tables have none; a missing eml.xml
# is reported once, by eml_present).
require_metadata <- function(dataset) {
  if (identical(dataset$source$type, "table")) skip_check("simple tables have no metadata")
  if (is.null(dataset$metadata)) skip_check("no eml.xml (see eml_present)")
  dataset$metadata
}

eml_issue <- function(dataset, field, message, value = NA_character_, suggestion = NA_character_) {
  new_issues(dataset, "eml", file = "eml.xml", field = field, value = value, message = message, suggestion = suggestion)
}

is_missing_text <- function(x) is.null(x) || length(x) == 0 || all(is.na(x) | !nzchar(trimws(x)))

email_pattern <- "^[^@\\s]+@[^@\\s]+\\.[A-Za-z]{2,}$"

word_count <- function(text) if (is_missing_text(text)) 0L else length(strsplit(trimws(text), "\\s+")[[1]])

# -----------------------------------------------------------------------------
# CHECK  eml_present
# What:  the archive contains an eml.xml metadata file.
# -----------------------------------------------------------------------------
check_eml_present <- function(dataset, settings) {
  if (identical(dataset$source$type, "table")) skip_check("simple tables have no metadata")
  if (!is.null(dataset$eml)) return(empty_issues())
  eml_issue(dataset, "eml.xml", "The archive has no metadata file (eml.xml)")
}

# -----------------------------------------------------------------------------
# CHECKS  eml_title_filled, eml_abstract_filled, eml_citation_filled
# What:   the metadata has a title / an abstract / a dataset citation.
# -----------------------------------------------------------------------------
check_eml_title_filled <- function(dataset, settings) {
  if (!is_missing_text(require_metadata(dataset)$title)) return(empty_issues())
  eml_issue(dataset, "dataset/title", "The metadata has no dataset title")
}

check_eml_abstract_filled <- function(dataset, settings) {
  if (!is_missing_text(require_metadata(dataset)$abstract)) return(empty_issues())
  eml_issue(dataset, "dataset/abstract", "The metadata has no abstract")
}

check_eml_citation_filled <- function(dataset, settings) {
  if (!is_missing_text(require_metadata(dataset)$citation)) return(empty_issues())
  eml_issue(dataset, "additionalMetadata/citation", "The metadata has no dataset citation")
}

# -----------------------------------------------------------------------------
# CHECKS  eml_contact_filled, eml_creator_filled, eml_metadata_provider_filled
# What:   a contact (name, institute, valid e-mail) / a creator / a metadata provider.
# -----------------------------------------------------------------------------
check_eml_contact_filled <- function(dataset, settings) {
  party_issues(dataset, "contact", "dataset/contact", need_email = TRUE, label = "point of contact")
}

check_eml_creator_filled <- function(dataset, settings) {
  party_issues(dataset, "creator", "dataset/creator", need_email = FALSE, label = "data creator")
}

check_eml_metadata_provider_filled <- function(dataset, settings) {
  party_issues(dataset, "metadataProvider", "dataset/metadataProvider", need_email = TRUE, label = "metadata provider")
}

party_issues <- function(dataset, wanted_role, element, need_email, label) {
  people <- require_metadata(dataset)$people
  parties <- people[role == wanted_role]   # 'role' is the column; the argument has another name on purpose
  named <- parties[!is.na(given_name) | !is.na(sur_name) | !is.na(organization)]
  if (!nrow(named)) {
    value <- if (nrow(parties)) "an entry without name or institute (placeholder?)" else NA_character_
    return(eml_issue(dataset, element, sprintf("The metadata has no %s with a name or institute", label), value = value))
  }
  if (!need_email) return(empty_issues())
  valid_email <- !is.na(named$email) & grepl(email_pattern, trimws(named$email), perl = TRUE)
  if (any(valid_email)) return(empty_issues())
  eml_issue(dataset, paste0(element, "/electronicMailAddress"), sprintf("The %s has no valid e-mail address", label),
            value = paste(ifelse(is.na(named$email), "(empty)", named$email), collapse = "; "))
}

# -----------------------------------------------------------------------------
# CHECK  eml_license_valid
# What:  a machine-readable licence: CC0, CC-BY or CC-BY-NC.
# -----------------------------------------------------------------------------
check_eml_license_valid <- function(dataset, settings) {
  license <- require_metadata(dataset)$license
  kind <- eml_license_kind(license)
  if (kind %in% c("CC0", "CC-BY", "CC-BY-NC")) return(empty_issues())
  if (kind == "missing") return(eml_issue(dataset, "dataset/intellectualRights", "The metadata has no licence"))
  eml_issue(dataset, "dataset/intellectualRights", "The licence is not CC0, CC-BY or CC-BY-NC",
            value = shorten(license$text), suggestion = "http://creativecommons.org/licenses/by/4.0/legalcode")
}

# The licence of the EML: URL first, then name, then text.
eml_license_kind <- function(license) {
  candidates <- c(license$url, license$name, license$text)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (!length(candidates)) return("missing")
  kinds <- license_kind(candidates)
  if (any(kinds != "other")) kinds[kinds != "other"][1] else "other"
}

# -----------------------------------------------------------------------------
# CHECKS  eml_geographic/temporal/taxonomic_coverage_filled, eml_keywords_filled
# What:   the coverages and keywords are filled in.
# -----------------------------------------------------------------------------
check_eml_geographic_coverage_filled <- function(dataset, settings) {
  boxes <- eml_boxes(list(metadata = require_metadata(dataset)), required = FALSE)
  if (nrow(boxes)) return(empty_issues())
  eml_issue(dataset, "dataset/coverage/geographicCoverage", "The metadata has no geographic coverage with a bounding box")
}

check_eml_temporal_coverage_filled <- function(dataset, settings) {
  temporal <- require_metadata(dataset)$temporal
  if (nrow(temporal) && any(!is.na(temporal$begin) | !is.na(temporal$single))) return(empty_issues())
  eml_issue(dataset, "dataset/coverage/temporalCoverage", "The metadata has no temporal coverage")
}

check_eml_taxonomic_coverage_filled <- function(dataset, settings) {
  taxonomic <- require_metadata(dataset)$taxonomic
  if (!is_missing_text(taxonomic$description) || nrow(taxonomic$taxa)) return(empty_issues())
  eml_issue(dataset, "dataset/coverage/taxonomicCoverage", "The metadata has no taxonomic coverage")
}

check_eml_keywords_filled <- function(dataset, settings) {
  keywords <- require_metadata(dataset)$keywords
  # The IPT adds 'Occurrence' / 'Samplingevent' from the GBIF dataset type vocabulary itself.
  themes <- keywords[!grepl("(?i)gbif dataset (type|subtype)", ifelse(is.na(thesaurus), "", thesaurus), perl = TRUE)]
  if (nrow(themes)) return(empty_issues())
  eml_issue(dataset, "dataset/keywordSet", "The metadata has no keywords or themes (apart from the GBIF dataset type)")
}

# -----------------------------------------------------------------------------
# CHECK  eml_persistent_identifier
# What:  a DOI or another persistent identifier for the dataset (FAIR F1).
# -----------------------------------------------------------------------------
check_eml_persistent_identifier <- function(dataset, settings) {
  metadata <- require_metadata(dataset)
  candidates <- c(metadata$doi, metadata$identifiers, metadata$package_id)
  persistent <- grepl("(?i)^(doi:|https?://(dx\\.)?doi\\.org/|10\\.\\d{4,}/|hdl:|https?://hdl\\.handle\\.net/|ark:|https?://n2t\\.net/)",
                      candidates, perl = TRUE)
  if (any(persistent)) return(empty_issues())
  eml_issue(dataset, "dataset/alternateIdentifier", "The dataset has no persistent identifier (DOI, handle, ...)",
            suggestion = "register a DOI (e.g. through the IPT, GBIF or the VLIZ IMIS catalogue)")
}

# -----------------------------------------------------------------------------
# CHECK  eml_people_have_orcid
# What:  persons listed as creator, contact or metadata provider have an ORCID.
# -----------------------------------------------------------------------------
check_eml_people_have_orcid <- function(dataset, settings) {
  people <- require_metadata(dataset)$people
  persons <- people[role %in% c("creator", "contact", "metadataProvider") & (!is.na(given_name) | !is.na(sur_name))]
  without <- persons[is.na(orcid)]
  if (!nrow(without)) return(empty_issues())
  names <- trimws(paste(ifelse(is.na(without$given_name), "", without$given_name), ifelse(is.na(without$sur_name), "", without$sur_name)))
  eml_issue(dataset, paste0("dataset/", without$role, "/userId"), "This person has no ORCID",
            value = paste0(names, " (", without$role, ")"))
}

# -----------------------------------------------------------------------------
# CHECK  eml_organisation_identifier
# What:  institutes listed on their own have an identifier (EDMO code or ROR).
# -----------------------------------------------------------------------------
check_eml_organisation_identifier <- function(dataset, settings) {
  people <- require_metadata(dataset)$people
  institutes <- people[is.na(given_name) & is.na(sur_name) & !is.na(organization) & is.na(other_ids) & is.na(orcid)]
  if (!nrow(institutes)) return(empty_issues())
  eml_issue(dataset, paste0("dataset/", institutes$role, "/organizationName"), "This institute has no identifier (EDMO or ROR)",
            value = institutes$organization)
}

# -----------------------------------------------------------------------------
# CHECK  eml_methods_described
# What:  the metadata describes how the data were collected (FAIR R1.2).
# -----------------------------------------------------------------------------
check_eml_methods_described <- function(dataset, settings) {
  methods <- require_metadata(dataset)$methods
  if (length(methods$steps) || !is_missing_text(methods$sampling) || !is_missing_text(methods$study_extent)) return(empty_issues())
  eml_issue(dataset, "dataset/methods", "The metadata does not describe the methods or the sampling")
}

# -----------------------------------------------------------------------------
# CHECK  eml_title_informative
# What:  the title has at least min_words words, is not only capitals and is not
#        just the IPT short name.
# Settings: min_words (5)
# -----------------------------------------------------------------------------
check_eml_title_informative <- function(dataset, settings) {
  metadata <- require_metadata(dataset)
  title <- metadata$title
  if (is_missing_text(title)) skip_check("no title (see eml_title_filled)")
  min_words <- setting(settings, "min_words", 5)
  short_name <- sub("^.*[?&]r=([^&]+).*$", "\\1", ifelse(is.na(metadata$ipt_url), "", metadata$ipt_url))
  problems <- c(
    if (word_count(title) < min_words) sprintf("has fewer than %d words", min_words),
    if (grepl("[A-Z]", title) && !grepl("[a-z]", title)) "is in capitals only",
    if (nzchar(short_name) && simple_name(title) == simple_name(short_name)) "is only the IPT short name")
  if (!length(problems)) return(empty_issues())
  eml_issue(dataset, "dataset/title", paste("The title", paste(problems, collapse = " and ")), value = title,
            suggestion = "say what, where and when, e.g. 'Macrobenthos of the Belgian part of the North Sea 2010-2020'")
}

# -----------------------------------------------------------------------------
# CHECK  eml_abstract_informative
# What:  the abstract has at least min_words words.
# Settings: min_words (50)
# -----------------------------------------------------------------------------
check_eml_abstract_informative <- function(dataset, settings) {
  abstract <- require_metadata(dataset)$abstract
  if (is_missing_text(abstract)) skip_check("no abstract (see eml_abstract_filled)")
  min_words <- setting(settings, "min_words", 50)
  words <- word_count(abstract)
  if (words >= min_words) return(empty_issues())
  eml_issue(dataset, "dataset/abstract", sprintf("The abstract has only %d words (at least %d expected)", words, min_words),
            value = shorten(abstract))
}

# -----------------------------------------------------------------------------
# CHECK  eml_language_given
# What:  the language of the data and of the metadata are given.
# -----------------------------------------------------------------------------
check_eml_language_given <- function(dataset, settings) {
  metadata <- require_metadata(dataset)
  combine_issues(list(
    if (is_missing_text(metadata$language)) eml_issue(dataset, "dataset/language", "The language of the dataset is not given"),
    if (is_missing_text(metadata$metadata_language)) eml_issue(dataset, "eml/@xml:lang", "The language of the metadata is not given")))
}

# -----------------------------------------------------------------------------
# CHECK  eml_no_placeholders
# What:  metadata texts are not placeholders ('-----', 'TBD', 'xxx', 'NA') and
#        do not say 'In press'.
# -----------------------------------------------------------------------------
check_eml_no_placeholders <- function(dataset, settings) {
  require_metadata(dataset)
  doc <- xml2::xml_ns_strip(xml2::read_xml(as.character(dataset$eml)))
  leaves <- xml2::xml_find_all(doc, "//*[not(*)]")
  texts <- trimws(xml2::xml_text(leaves))
  keep <- nzchar(texts)
  leaves <- leaves[keep]
  texts <- texts[keep]
  placeholder <- grepl("(?i)^(-{2,}|\\.{2,}|_{2,}|x{3,}|tbd|tba|todo|to do|n/?a|null|none|unknown|to be (added|completed|defined|done))$", texts, perl = TRUE)
  in_press <- grepl("(?i)\\bin press\\b", texts, perl = TRUE)
  rows <- which(placeholder | in_press)
  if (!length(rows)) return(empty_issues())
  eml_issue(dataset, sub("^/[^/]*", "", xml2::xml_path(leaves[rows])),   # drop the root: /dataset/creator/...
            ifelse(placeholder[rows], "Placeholder text in the metadata", "The metadata says 'In press': is it published now?"),
            value = shorten(texts[rows], 80))
}

# -----------------------------------------------------------------------------
# CHECK  ipt_url_length
# What:  the IPT resource URL has at most 255 characters.
# Why:   longer URLs break the EurOBIS database and data flows.
# Settings: max_characters (255)
# -----------------------------------------------------------------------------
check_ipt_url_length <- function(dataset, settings) {
  max_characters <- setting(settings, "max_characters", 255)
  metadata <- require_metadata(dataset)
  urls <- unique(without_na(c(metadata$ipt_url, dataset$source$archive_url)))
  long <- urls[nchar(urls) > max_characters]
  if (!length(long)) return(empty_issues())
  eml_issue(dataset, "dataset/alternateIdentifier", sprintf("The IPT resource URL is longer than %s characters", max_characters),
            value = sprintf("%s (%d characters)", shorten(long, 80), nchar(long)))
}

# -----------------------------------------------------------------------------
# CHECK  license_data_matches_eml
# What:  a license column in the data matches the licence in the metadata.
# -----------------------------------------------------------------------------
check_license_data_matches_eml <- function(dataset, settings) {
  eml_kind <- eml_license_kind(require_metadata(dataset)$license)
  if (eml_kind %in% c("missing", "other")) skip_check("no accepted licence in the metadata (see eml_license_valid)")
  tables <- tables_with_fields(dataset, "license")
  if (!length(tables)) skip_check("no license column in the data")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$license
    rows <- rows_where(values, function(v) license_kind(v) != eml_kind & !is_placeholder(v))
    new_issues(dataset, table_name, rows, field = "license", value = values[rows],
               message = "The licence in the data differs from the licence in the metadata",
               suggestion = paste("metadata:", eml_kind))
  }))
}
