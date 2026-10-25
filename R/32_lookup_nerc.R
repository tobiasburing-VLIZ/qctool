# =============================================================================
# Lookups in the NERC Vocabulary Server (NVS), via its SPARQL endpoint
#   https://vocab.nerc.ac.uk/sparql/
#
#   nerc_terms()   for each term URI: found, label, alternative labels, deprecated,
#                  replacement, the P06 units NERC links to it (for P01 terms)
#                  and its unit dimension (P24, for P06 units).
#
# URIs are looked up in their standard form
# http://vocab.nerc.ac.uk/collection/<collection>/current/<code>/ - other forms
# ("https", no trailing slash) are reported by nvs_uri_format and looked up
# as if written in the standard form.
# 40 terms per request (GET; the endpoint closes POST connections).
# Common part (cache, offline, retries): 30_lookup.R
# =============================================================================

nerc_sparql_url <- "https://vocab.nerc.ac.uk/sparql/sparql"

# The standard form of a NERC URI; NA when the value is not a NERC term URI.
nerc_canonical <- function(uri) {
  per_value(uri, function(u) {
    collection <- nerc_collection(u)
    code <- nerc_code(u)
    fifelse(is.na(collection) | is.na(code), NA_character_,
            paste0("http://vocab.nerc.ac.uk/collection/", collection, "/current/", code, "/"))
  })
}

# One row per URI (give them in standard form, see nerc_canonical()).
nerc_terms <- function(dataset, uris) {
  lookup_values(dataset, "nerc_terms", uris, fetch_nerc_terms, batch_size = 40, what = "NERC terms")
}

nerc_query <- function(uris) {
  paste0(
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>\n",
    "PREFIX dcterms: <http://purl.org/dc/terms/>\n",
    "SELECT ?uri ?label ?alt ?deprecated ?replaced_by ?unit ?dimension WHERE {\n",
    "  VALUES ?uri { ", paste0("<", uris, ">", collapse = " "), " }\n",
    "  ?uri skos:prefLabel ?label .\n",
    "  OPTIONAL { ?uri skos:altLabel ?alt }\n",
    "  OPTIONAL { ?uri owl:deprecated ?deprecated }\n",
    "  OPTIONAL { ?uri dcterms:isReplacedBy ?replaced_by }\n",
    "  OPTIONAL { ?uri skos:related ?unit . FILTER(STRSTARTS(STR(?unit), \"http://vocab.nerc.ac.uk/collection/P06/\")) }\n",
    "  OPTIONAL { ?uri skos:broader ?dimension . FILTER(STRSTARTS(STR(?dimension), \"http://vocab.nerc.ac.uk/collection/P24/\")) }\n",
    "}")
}

fetch_nerc_terms <- function(batches) {
  requests <- lapply(batches, function(uris) {
    list(url = paste0(nerc_sparql_url, "?query=", curl::curl_escape(nerc_query(uris)), "&output=json"),
         accept = "application/sparql-results+json")
  })
  fetch_and_parse(batches, requests, "the NERC Vocabulary Server", parse_nerc_terms)
}

# SPARQL gives one row per combination of label, alternative label, unit, ...;
# here they are folded into one row per term (several values joined with " | ").
parse_nerc_terms <- function(uris, response) {
  bindings <- jsonlite::fromJSON(response_text(response, "the NERC Vocabulary Server"))$results$bindings
  value_of <- function(name) {
    if (!length(bindings) || is.null(bindings[[name]])) return(rep(NA_character_, NROW(bindings)))
    bindings[[name]]$value
  }
  rows <- data.table(query = value_of("uri"), label = value_of("label"), alt = value_of("alt"),
                     deprecated = value_of("deprecated"), replaced_by = value_of("replaced_by"),
                     unit = value_of("unit"), dimension = value_of("dimension"))
  joined <- function(x) if (all(is.na(x))) NA_character_ else paste(sort(unique(without_na(x))), collapse = " | ")
  terms <- rows[, .(label = label[1], alt_labels = joined(alt), deprecated = any(deprecated %in% "true"),
                    replaced_by = joined(replaced_by), linked_units = joined(unit), dimension = joined(dimension)), by = query]
  result <- data.table(query = uris, found = uris %chin% terms$query, collection = nerc_collection(uris))
  merge(result, terms, by = "query", all.x = TRUE, sort = FALSE)
}

# " | "-joined values back to a vector.
split_joined <- function(x) if (is.na(x)) character() else strsplit(x, " | ", fixed = TRUE)[[1]]
