# =============================================================================
# Reference tables in inst/reference/
#
#   dwc_terms.csv     the EurOBIS term table: every known term with its IRI,
#                     data type, database column size, and the tables where
#                     EurOBIS expects it (copied from dwc_terms_datatypes_eurobis.csv)
#   placeholders.csv  text used instead of an empty cell (NA, NULL, -, ...)
#   term_synonyms.csv frequent wrong column names and the right term (rank -> taxonRank)
#
# The tables are read once per R session and kept in memory.
# =============================================================================

reference_cache <- new.env(parent = emptyenv())

# Read a CSV from inst/reference/ (all columns as text).
reference_table <- function(name) {
  if (is.null(reference_cache[[name]])) {
    path <- system.file("reference", name, package = "qctool", mustWork = TRUE)
    reference_cache[[name]] <- fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  }
  reference_cache[[name]]
}

term_table <- function() reference_table("dwc_terms.csv")

# The usual name of a term, from its IRI:
#   "http://rs.tdwg.org/dwc/terms/decimalLatitude" -> "decimalLatitude"
#   "http://rs.gbif.org/terms/dna_sequence"        -> "DNA_sequence" (name from the term table)
# Unknown IRIs: the last part after "/" or "#".
term_name_from_iri <- function(iri) {
  terms <- term_table()
  known <- terms$term[match(same_namespace(iri), same_namespace(terms$term_iri))]
  fallback <- sub("^.*[/#]", "", iri)
  ifelse(is.na(known), fallback, known)
}

is_known_term_iri <- function(iri) same_namespace(iri) %chin% same_namespace(term_table()$term_iri)

# Dublin Core terms are written with two namespaces that mean the same:
# http://purl.org/dc/terms/ (used by Darwin Core and the IPT) and
# http://purl.org/dc/elements/1.1/ (used for 'type' and 'language' in the
# EurOBIS term table). Compare them as one.
same_namespace <- function(iri) {
  sub("^http://purl.org/dc/elements/1.1/", "http://purl.org/dc/terms/", iri)
}

# Database column size from the SQL type: "nvarchar(255)" -> 255; max / numbers -> NA
database_length_limit <- function(sql_type) {
  limit <- suppressWarnings(as.integer(sub("^n?varchar\\((\\d+)\\)$", "\\1", sql_type)))
  ifelse(grepl("^n?varchar\\(\\d+\\)$", sql_type), limit, NA_integer_)
}
