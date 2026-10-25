# =============================================================================
# Reading the metadata (eml.xml)
#
# The EML is turned into plain R lists and tables in dataset$metadata, so the
# metadata and FAIR checks (milestone 4a-4) only need to look things up:
#
#   title, abstract, language, pub_date, package_id
#   identifiers      all alternateIdentifiers; ipt_url and doi picked out
#   people           one row per person/institute: role (creator, contact,
#                    metadataProvider, associatedParty, personnel), names,
#                    organisation, position, email, orcid, other user ids
#   license          text, url, name (from intellectualRights and EML 2.2 'licensed')
#   citation         dataset citation; bibliography = cited publications
#   geographic       one row per area: description, west, east, north, south
#   temporal         one row per period: begin, end (or single date)
#   taxonomic        description + one row per taxon: rank, name, common name
#   keywords         one row per keyword: keyword, thesaurus
#   methods          method steps, study extent, sampling, quality control
#   project          title, funding, identifier
#   maintenance      description, update frequency
#
# Missing parts are NA or empty tables, never errors: reporting them is the
# job of the metadata checks.
# =============================================================================

read_eml_into <- function(dataset, folder, eml_file) {
  if (is.na(eml_file) || !nzchar(eml_file)) eml_file <- "eml.xml"
  path <- file.path(folder, eml_file)
  if (!file.exists(path)) return(dataset)   # reported by the check eml_present

  doc <- tryCatch(xml2::read_xml(path), error = function(e) e)
  if (inherits(doc, "error")) {
    return(add_problem(dataset, eml_file, paste("eml.xml is not valid XML:", conditionMessage(doc))))
  }
  dataset$eml <- doc            # kept as read (with namespaces), for checks that need the original
  dataset$metadata <- eml_metadata(doc)
  dataset$info <- list(title = dataset$metadata$title, ipt_url = dataset$metadata$ipt_url,
                       identifiers = dataset$metadata$identifiers, pub_date = dataset$metadata$pub_date)
  dataset
}

#' Read the metadata of an EML document
#'
#' @param doc An EML document (from `xml2::read_xml()`) or a path to eml.xml.
#' @return A list with the parts described in `R/13_read_eml.R`.
#' @export
qc_read_eml <- function(doc) {
  if (is.character(doc)) doc <- xml2::read_xml(doc)
  eml_metadata(doc)
}

eml_metadata <- function(doc) {
  doc <- xml2::xml_ns_strip(xml2::read_xml(as.character(doc)))   # a copy without namespaces
  dataset <- xml2::xml_find_first(doc, "//dataset")
  identifiers <- texts(dataset, "./alternateIdentifier")

  list(
    title             = text_of(dataset, "./title"),
    abstract          = paragraphs(dataset, "./abstract"),
    language          = text_of(dataset, "./language"),
    metadata_language = attribute_of(xml2::xml_root(doc), "lang"),
    pub_date          = text_of(dataset, "./pubDate"),
    package_id        = attribute_of(xml2::xml_root(doc), "packageId"),
    identifiers       = identifiers,
    ipt_url           = first_or_na(grep("resource(\\.do)?\\?r=", identifiers, value = TRUE)),
    doi               = first_or_na(grep("^(doi:|https?://(dx\\.)?doi\\.org/|10\\.)", identifiers, value = TRUE, ignore.case = TRUE)),
    people            = eml_people(dataset),
    license           = eml_license(dataset),
    citation          = first_or_na(c(text_of(doc, "//additionalMetadata//citation"), text_of(dataset, "./citation"))),
    bibliography      = texts(doc, "//additionalMetadata//bibliography/citation"),
    geographic        = eml_geographic(dataset),
    temporal          = eml_temporal(dataset),
    taxonomic         = eml_taxonomic(dataset),
    keywords          = eml_keywords(dataset),
    methods           = list(steps = vapply(xml2::xml_find_all(dataset, "./methods/methodStep"),
                                            function(step) paragraphs(step, "./description"), character(1)),
                             study_extent = paragraphs(dataset, "./methods/sampling/studyExtent/description"),
                             sampling = paragraphs(dataset, "./methods/sampling/samplingDescription"),
                             quality_control = paragraphs(dataset, "./methods/qualityControl/description")),
    project           = list(title = text_of(dataset, "./project/title"),
                             funding = paragraphs(dataset, "./project/funding"),
                             identifier = attribute_of(xml2::xml_find_first(dataset, "./project"), "id")),
    maintenance       = list(description = paragraphs(dataset, "./maintenance/description"),
                             frequency = text_of(dataset, "./maintenance/maintenanceUpdateFrequency"))
  )
}

# --- Parts -------------------------------------------------------------------------

eml_people <- function(dataset) {
  roles <- c("creator", "contact", "metadataProvider", "associatedParty", "project/personnel")
  rows <- list()
  for (role in roles) {
    for (node in xml2::xml_find_all(dataset, paste0("./", role))) {
      user_ids <- xml2::xml_find_all(node, "./userId")
      directories <- tolower(xml2::xml_attr(user_ids, "directory"))
      ids <- trimws(xml2::xml_text(user_ids))
      is_orcid <- grepl("orcid", directories) | grepl("orcid\\.org|^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", ids)
      rows[[length(rows) + 1]] <- data.table(
        role         = sub("^project/", "", role),
        given_name   = text_of(node, "./individualName/givenName"),
        sur_name     = text_of(node, "./individualName/surName"),
        organization = text_of(node, "./organizationName"),
        position     = text_of(node, "./positionName"),
        email        = text_of(node, "./electronicMailAddress"),
        orcid        = first_or_na(ids[is_orcid]),
        other_ids    = if (any(!is_orcid)) paste(ids[!is_orcid], collapse = "; ") else NA_character_,
        party_role   = text_of(node, "./role"))
    }
  }
  if (!length(rows)) {
    return(data.table(role = character(), given_name = character(), sur_name = character(), organization = character(),
                      position = character(), email = character(), orcid = character(), other_ids = character(),
                      party_role = character()))
  }
  rbindlist(rows)
}

eml_license <- function(dataset) {
  rights <- xml2::xml_find_first(dataset, "./intellectualRights")
  list(
    text = paragraphs(dataset, "./intellectualRights"),
    url  = first_or_na(c(attribute_of(xml2::xml_find_first(rights, ".//ulink"), "url"), text_of(dataset, "./licensed/url"))),
    name = first_or_na(c(text_of(rights, ".//citetitle"), text_of(dataset, "./licensed/licenseName")))
  )
}

eml_geographic <- function(dataset) {
  nodes <- xml2::xml_find_all(dataset, "./coverage/geographicCoverage")
  number <- function(node, name) suppressWarnings(as.numeric(text_of(node, paste0("./boundingCoordinates/", name))))
  data.table(
    description = vapply(nodes, text_of, character(1), xpath = "./geographicDescription"),
    west  = vapply(nodes, number, numeric(1), name = "westBoundingCoordinate"),
    east  = vapply(nodes, number, numeric(1), name = "eastBoundingCoordinate"),
    north = vapply(nodes, number, numeric(1), name = "northBoundingCoordinate"),
    south = vapply(nodes, number, numeric(1), name = "southBoundingCoordinate"))
}

eml_temporal <- function(dataset) {
  nodes <- xml2::xml_find_all(dataset, "./coverage/temporalCoverage")
  data.table(
    begin  = vapply(nodes, text_of, character(1), xpath = "./rangeOfDates/beginDate/calendarDate"),
    end    = vapply(nodes, text_of, character(1), xpath = "./rangeOfDates/endDate/calendarDate"),
    single = vapply(nodes, text_of, character(1), xpath = "./singleDateTime/calendarDate"))
}

eml_taxonomic <- function(dataset) {
  taxa <- xml2::xml_find_all(dataset, "./coverage/taxonomicCoverage//taxonomicClassification")
  list(
    description = paste(texts(dataset, "./coverage/taxonomicCoverage/generalTaxonomicCoverage"), collapse = "\n\n"),
    taxa = data.table(
      rank        = vapply(taxa, text_of, character(1), xpath = "./taxonRankName"),
      name        = vapply(taxa, text_of, character(1), xpath = "./taxonRankValue"),
      common_name = vapply(taxa, text_of, character(1), xpath = "./commonName")))
}

eml_keywords <- function(dataset) {
  sets <- xml2::xml_find_all(dataset, "./keywordSet")
  rows <- lapply(sets, function(set) {
    keywords <- texts(set, "./keyword")
    if (!length(keywords)) return(NULL)
    data.table(keyword = keywords, thesaurus = text_of(set, "./keywordThesaurus"))
  })
  result <- rbindlist(rows)
  if (!nrow(result)) result <- data.table(keyword = character(), thesaurus = character())
  result
}

# --- Small XML helpers ---------------------------------------------------------------

# Text of the first matching node, trimmed; NA when missing or empty.
text_of <- function(node, xpath) {
  found <- xml2::xml_find_first(node, xpath)
  value <- trimws(xml2::xml_text(found))
  if (length(value) == 0 || is.na(value) || !nzchar(value)) NA_character_ else value
}

# Texts of all matching nodes, trimmed, without empty ones.
texts <- function(node, xpath) {
  values <- trimws(xml2::xml_text(xml2::xml_find_all(node, xpath)))
  values[nzchar(values)]
}

# Text of an element made of <para> parts (abstract, rights, ...), paragraphs
# separated by an empty line; NA when empty.
paragraphs <- function(node, xpath) {
  element <- xml2::xml_find_first(node, xpath)
  if (inherits(element, "xml_missing")) return(NA_character_)
  parts <- texts(element, "./para")
  if (!length(parts)) parts <- trimws(xml2::xml_text(element))
  value <- paste(parts[nzchar(parts)], collapse = "\n\n")
  if (nzchar(value)) value else NA_character_
}

attribute_of <- function(node, name) {
  if (inherits(node, "xml_missing")) return(NA_character_)
  value <- xml2::xml_attr(node, name)
  if (is.na(value) && name == "lang") value <- xml2::xml_attr(node, "xml:lang")
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

first_or_na <- function(values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values)) values[1] else NA_character_
}
