# =============================================================================
# Lookups in WoRMS (World Register of Marine Species), REST API
#   https://www.marinespecies.org/rest/
#
#   worms_records()       AphiaRecordsByAphiaIDs: the record of each AphiaID
#                         (50 per request; unknown IDs come back empty)
#   worms_name_matches()  AphiaRecordsByMatchNames: WoRMS matches for names
#                         (taxamatch: exact, phonetic, near_1, ...; 50 per request)
# Common part (cache, offline, retries): 30_lookup.R
# =============================================================================

worms_url <- "https://www.marinespecies.org/rest/"

# The AphiaID in a scientificNameID, also from forms the format check reports
# ("117081", a WoRMS URL, an LSID with spaces). NA when there is none.
aphia_id <- function(scientific_name_id) {
  per_value(scientific_name_id, function(v) sub("^.*:", "", suggest_worms_lsid(v)))
}

lsid_of <- function(aphia_id) ifelse(is.na(aphia_id), NA_character_, paste0("urn:lsid:marinespecies.org:taxname:", aphia_id))

# One row per AphiaID: found, scientificname, authority, status, rank, rank_id,
# valid_id, valid_name, is_marine, is_brackish, is_freshwater, is_terrestrial,
# kingdom ... genus.
worms_records <- function(dataset, aphia_ids) {
  lookup_values(dataset, "worms_records", as.character(aphia_ids), fetch_worms_records, batch_size = 50, what = "AphiaIDs")
}

fetch_worms_records <- function(batches) {
  requests <- lapply(batches, function(ids) {
    list(url = paste0(worms_url, "AphiaRecordsByAphiaIDs?", paste0("aphiaids[]=", ids, collapse = "&")))
  })
  fetch_and_parse(batches, requests, "WoRMS", parse_worms_records)
}

parse_worms_records <- function(ids, response) {
  text <- response_text(response, "WoRMS", ok = c(200, 204))   # 204: none of the IDs exists
  records <- if (nzchar(text)) jsonlite::fromJSON(text, simplifyVector = FALSE) else list()
  records <- Filter(Negate(is.null), records)
  rows <- rbindlist(lapply(records, worms_record_row), fill = TRUE)
  result <- data.table(query = ids, found = ids %chin% rows$query)
  if (nrow(rows)) result <- merge(result, rows, by = "query", all.x = TRUE, sort = FALSE)
  result
}

worms_record_row <- function(record) {
  flag <- function(name) as.integer(json_field(record, name, NA_integer_))
  data.table(
    query = as.character(record$AphiaID),
    scientificname = json_field(record, "scientificname", NA_character_),
    authority      = json_field(record, "authority", NA_character_),
    status         = json_field(record, "status", NA_character_),
    rank           = json_field(record, "rank", NA_character_),
    rank_id        = as.integer(json_field(record, "taxonRankID", NA_integer_)),
    valid_id       = as.character(json_field(record, "valid_AphiaID", NA_character_)),
    valid_name     = json_field(record, "valid_name", NA_character_),
    is_marine      = flag("isMarine"),
    is_brackish    = flag("isBrackish"),
    is_freshwater  = flag("isFreshwater"),
    is_terrestrial = flag("isTerrestrial"),
    kingdom = json_field(record, "kingdom", NA_character_), phylum = json_field(record, "phylum", NA_character_),
    class   = json_field(record, "class", NA_character_),   order  = json_field(record, "order", NA_character_),
    family  = json_field(record, "family", NA_character_),  genus  = json_field(record, "genus", NA_character_))
}

# One row per name: found (any match), n_matches, the first match (id,
# scientificname, match_type, status, valid_id, valid_name) and the names of
# all matches when there are several. Name matching is slow in WoRMS: 25 names
# per request, 2 minutes allowed per request.
worms_name_matches <- function(dataset, names) {
  lookup_values(dataset, "worms_names", names, fetch_worms_names, batch_size = 25, what = "scientific names")
}

fetch_worms_names <- function(batches) {
  requests <- lapply(batches, function(names) {
    list(url = paste0(worms_url, "AphiaRecordsByMatchNames?",
                      paste0("scientificnames[]=", curl::curl_escape(names), collapse = "&"), "&marine_only=false"),
         timeout = 120)
  })
  fetch_and_parse(batches, requests, "WoRMS", parse_worms_names)
}

parse_worms_names <- function(names, response) {
  text <- response_text(response, "WoRMS", ok = c(200, 204))
  matches <- if (nzchar(text)) jsonlite::fromJSON(text, simplifyVector = FALSE) else list()
  rbindlist(lapply(seq_along(names), function(i) {
    found <- if (i <= length(matches)) Filter(Negate(is.null), matches[[i]]) else list()
    if (!length(found)) return(data.table(query = names[i], found = FALSE, n_matches = 0L))
    first <- found[[1]]
    data.table(query = names[i], found = TRUE, n_matches = length(found),
               id = as.character(first$AphiaID), scientificname = json_field(first, "scientificname", NA_character_),
               match_type = json_field(first, "match_type", NA_character_), status = json_field(first, "status", NA_character_),
               valid_id = as.character(json_field(first, "valid_AphiaID", NA_character_)),
               valid_name = json_field(first, "valid_name", NA_character_),
               all_names = paste(vapply(found, function(m) paste0(json_field(m, "scientificname", "?"), " (", m$AphiaID, ")"), character(1)),
                                 collapse = "; "))
  }), fill = TRUE)
}
