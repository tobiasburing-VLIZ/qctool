# =============================================================================
# Checks, group "emof": against the NERC Vocabulary Server (online, cached)
#
#   measurementtypeid_valid    measurementunitid_valid    measurementvalueid_valid
#   vocabulary_terms_not_deprecated                       type_unit_compatible
#   unit_text_matches_unitid   measurementtype_matches_typeid
#
# Lookups: R/32_lookup_nerc.R (each different term once, cached 30 days).
# Only NERC URIs are looked up; URIs of other vocabularies are not judged here.
# Without internet these checks are "not performed", unless the cache knows
# all terms already. The offline eMoF checks are in checks_09_emof.R.
# List: inst/reference/unit_synonyms.csv (other ways to write a P06 unit)
# =============================================================================

# For one eMoF column: the standard URI per row (NA when it is not a NERC URI)
# and the NVS answer for each different URI.
emof_nerc_terms <- function(dataset, field) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, field)
  uri <- nerc_canonical(data[[field]])
  if (all(is.na(uri))) skip_check(sprintf("no NERC URIs in %s", field))
  list(name = name, field = field, data = data, uri = uri, terms = nerc_terms(dataset, unique(uri)))
}

# Issues for the rows whose term is one of 'selected' (rows of terms).
term_issues <- function(dataset, nerc, selected, message, suggestion = NULL) {
  rows <- which(nerc$uri %chin% selected$query)
  term <- selected[match(nerc$uri[rows], selected$query)]
  new_issues(dataset, nerc$name, rows, field = nerc$field, value = nerc$data[[nerc$field]][rows], message = message,
             suggestion = if (is.null(suggestion)) NA_character_ else suggestion(term))
}

# -----------------------------------------------------------------------------
# CHECK  measurementtypeid_valid / measurementunitid_valid / measurementvalueid_valid
# What:  the NERC term exists; a unit ID is a P06 unit; a type ID is not a unit.
# -----------------------------------------------------------------------------
check_measurementtypeid_valid <- function(dataset, settings) {
  nerc <- emof_nerc_terms(dataset, "measurementTypeID")
  combine_issues(list(
    term_issues(dataset, nerc, nerc$terms[found == FALSE], "measurementTypeID does not exist in the NERC Vocabulary Server"),
    term_issues(dataset, nerc, nerc$terms[found == TRUE & collection == "P06"],
                "measurementTypeID is a unit (P06), not a measurement type")))
}

check_measurementunitid_valid <- function(dataset, settings) {
  nerc <- emof_nerc_terms(dataset, "measurementUnitID")
  combine_issues(list(
    term_issues(dataset, nerc, nerc$terms[found == FALSE], "measurementUnitID does not exist in the NERC Vocabulary Server"),
    term_issues(dataset, nerc, nerc$terms[found == TRUE & collection != "P06"],
                "measurementUnitID is not a unit: units come from the P06 collection",
                suggestion = function(term) paste("this is a term of collection", term$collection))))
}

check_measurementvalueid_valid <- function(dataset, settings) {
  nerc <- emof_nerc_terms(dataset, "measurementValueID")
  term_issues(dataset, nerc, nerc$terms[found == FALSE], "measurementValueID does not exist in the NERC Vocabulary Server")
}

# -----------------------------------------------------------------------------
# CHECK  vocabulary_terms_not_deprecated
# What:  measurementTypeID, measurementUnitID and measurementValueID are not
#        deprecated; the replacement NERC gives is the suggestion.
# -----------------------------------------------------------------------------
check_vocabulary_terms_not_deprecated <- function(dataset, settings) {
  name <- require_emof(dataset)
  fields <- intersect(c("measurementTypeID", "measurementUnitID", "measurementValueID"), names(dataset$tables[[name]]))
  if (!any(vapply(fields, function(f) any(!is.na(nerc_canonical(dataset$tables[[name]][[f]]))), logical(1)))) {
    skip_check("no NERC URIs in measurementTypeID, measurementUnitID or measurementValueID")
  }
  combine_issues(lapply(fields, function(field) {
    nerc <- tryCatch(emof_nerc_terms(dataset, field), qc_not_applicable = function(condition) NULL)
    if (is.null(nerc)) return(NULL)
    term_issues(dataset, nerc, nerc$terms[found == TRUE & deprecated == TRUE], "This NERC term is deprecated",
                suggestion = function(term) fifelse(is.na(term$replaced_by), "NERC gives no replacement", term$replaced_by))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  type_unit_compatible
# What:  measurementUnitID is a unit that NERC links to the P01 measurementTypeID.
#        A unit of the same kind (e.g. cm where NERC links mm: both length,
#        P24 LENGTH) is only info - common and convertible; a unit of another
#        kind is an error.
# -----------------------------------------------------------------------------
check_type_unit_compatible <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, c("measurementTypeID", "measurementUnitID"))
  type_uri <- nerc_canonical(data$measurementTypeID)
  unit_uri <- nerc_canonical(data$measurementUnitID)

  # Each different (type, unit) pair once.
  pair <- value_codes(type_uri) * (nrow(data) + 1) + value_codes(unit_uri)
  first <- which(!duplicated(pair) & !is.na(type_uri) & !is.na(unit_uri) & nerc_collection(type_uri) %chin% "P01")
  if (!length(first)) skip_check("no P01 measurementTypeID with a NERC measurementUnitID")
  pairs <- data.table(pair = pair[first], type = type_uri[first], unit = unit_uri[first])
  types <- nerc_terms(dataset, unique(pairs$type))
  pairs[, `:=`(type_label = types$label[match(type, types$query)], linked = types$linked_units[match(type, types$query)])]
  pairs <- pairs[!is.na(linked) & !mapply(function(u, l) u %in% split_joined(l), unit, linked)]
  if (!nrow(pairs)) return(empty_issues())

  unit_terms <- nerc_terms(dataset, unique(c(pairs$unit, unlist(lapply(pairs$linked, split_joined)))))
  unit_term <- function(uris) unit_terms[match(uris, unit_terms$query)]
  pairs[, `:=`(unit_found = unit_term(unit)$found, unit_label = unit_term(unit)$label, unit_kind = unit_term(unit)$dimension)]
  pairs <- pairs[unit_found %in% TRUE]       # unknown units: measurementunitid_valid
  if (!nrow(pairs)) return(empty_issues())
  pairs[, linked_labels := vapply(linked, function(l) paste(unit_term(split_joined(l))$label, collapse = " or "), character(1))]
  pairs[, same_kind := mapply(function(kind, l) !is.na(kind) && kind %in% unit_term(split_joined(l))$dimension, unit_kind, linked)]

  rows <- which(pair %in% pairs$pair)
  index <- match(pair[rows], pairs$pair)   # outside pairs[...]: there 'pair' would mean its column
  p <- pairs[index]
  new_issues(dataset, name, rows, field = "measurementUnitID",
             value = sprintf("%s in %s", p$type_label, p$unit_label),
             message = ifelse(p$same_kind,
                              paste("NERC links this measurement type to", p$linked_labels, "- this unit measures the same kind of quantity, but is not the linked unit"),
                              paste("NERC links this measurement type to", p$linked_labels, "- not to this unit")),
             suggestion = vapply(p$linked, function(l) split_joined(l)[1], character(1)),
             severity_hint = ifelse(p$same_kind, "info", NA_character_))
}

# -----------------------------------------------------------------------------
# CHECK  unit_text_matches_unitid
# What:  measurementUnit (text) means the same as the NERC label of
#        measurementUnitID: 'cm' with Centimetres is fine, 'ind/m2' with
#        'Number per square metre' too; 'ind L-1' with 'per cubic metre' is not.
# How:   both written in a comparable form (normalise_unit); other accepted
#        writings per unit: inst/reference/unit_synonyms.csv.
# -----------------------------------------------------------------------------
check_unit_text_matches_unitid <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, c("measurementUnit", "measurementUnitID"))
  text <- data$measurementUnit
  unit_uri <- nerc_canonical(data$measurementUnitID)

  pair <- value_codes(text) * (nrow(data) + 1) + value_codes(unit_uri)
  first <- which(!duplicated(pair) & !is.na(unit_uri) & !is_empty_value(text))
  if (!length(first)) skip_check("no measurementUnit text together with a NERC measurementUnitID")
  pairs <- data.table(pair = pair[first], text = text[first], unit = unit_uri[first])
  unit_terms <- nerc_terms(dataset, unique(pairs$unit))
  term <- unit_terms[match(pairs$unit, unit_terms$query)]
  pairs[, `:=`(found = term$found, label = term$label, alt = term$alt_labels)]
  pairs <- pairs[found %in% TRUE]
  if (!nrow(pairs)) return(empty_issues())
  pairs[, same := mapply(unit_text_matches, text, label, alt, nerc_code(unit))]
  wrong <- pairs[same == FALSE]
  if (!nrow(wrong)) return(empty_issues())

  rows <- which(pair %in% wrong$pair)
  index <- match(pair[rows], wrong$pair)   # outside wrong[...]: there 'pair' would mean its column
  w <- wrong[index]
  new_issues(dataset, name, rows, field = "measurementUnit",
             value = sprintf("'%s' with %s", w$text, w$label),
             message = "measurementUnit does not mean the same as the NERC label of measurementUnitID",
             suggestion = ifelse(is.na(w$alt), w$label, sub(" \\|.*$", "", w$alt)))
}

unit_text_matches <- function(text, label, alt, code) {
  synonyms <- reference_table("unit_synonyms.csv")
  extra <- split_list(paste(synonyms$also_written_as[synonyms$unit_code == code], collapse = ";"))
  normalise_unit(text) %in% normalise_unit(c(label, split_joined(alt), extra))
}

# A unit in a form that can be compared: "Individuals per m2", "ind/m²",
# "ind m-2" and "Number per square metre" all become "/m^2" (the count word
# is dropped); "Centimeters" and "centimetres" both become "centimetre".
normalise_unit <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("²", "^2", x, fixed = TRUE)
  x <- gsub("³", "^3", x, fixed = TRUE)
  x <- gsub("[µμ]", "u", x)
  x <- gsub("meter", "metre", x, fixed = TRUE)
  x <- gsub("liter", "litre", x, fixed = TRUE)
  x <- gsub("\\b([a-z]{3,})s\\b", "\\1", x, perl = TRUE)                       # plural
  x <- gsub("\\bsquare ([a-z]+)", "\\1^2", x, perl = TRUE)
  x <- gsub("\\bcubic ([a-z]+)", "\\1^3", x, perl = TRUE)
  x <- gsub("\\bmetre\\b", "m", x, perl = TRUE)
  x <- gsub("\\b(individual|ind|specimen|number|nb|no|n|count)\\b|#", "", x, perl = TRUE)
  x <- gsub("\\s*\\bper\\b\\s*", "/", x, perl = TRUE)
  x <- gsub("\\s*([a-z]+)\\^?-(\\d)", "/\\1^\\2", x, perl = TRUE)              # m-2 -> /m^2
  x <- gsub("([a-z])(\\d)", "\\1^\\2", x, perl = TRUE)                          # m2 -> m^2
  x <- gsub("[[:space:].·*]", "", x)
  gsub("/+", "/", x)
}

# -----------------------------------------------------------------------------
# CHECK  measurementtype_matches_typeid
# What:  the measurementType text resembles the NERC label of the
#        measurementTypeID: at least one word in common, or a misspelling of
#        one ('Weigth'). P01 labels are long, so this starts as info.
# -----------------------------------------------------------------------------
check_measurementtype_matches_typeid <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, c("measurementType", "measurementTypeID"))
  text <- data$measurementType
  type_uri <- nerc_canonical(data$measurementTypeID)

  pair <- value_codes(text) * (nrow(data) + 1) + value_codes(type_uri)
  first <- which(!duplicated(pair) & !is.na(type_uri) & !is_empty_value(text))
  if (!length(first)) skip_check("no measurementType text together with a NERC measurementTypeID")
  pairs <- data.table(pair = pair[first], text = text[first], type = type_uri[first])
  types <- nerc_terms(dataset, unique(pairs$type))
  term <- types[match(pairs$type, types$query)]
  pairs[, `:=`(found = term$found, label = term$label, alt = term$alt_labels)]
  pairs <- pairs[found %in% TRUE]
  if (!nrow(pairs)) return(empty_issues())
  pairs[, verdict := mapply(compare_type_text, text, label, alt)]
  wrong <- pairs[verdict != "same"]
  if (!nrow(wrong)) return(empty_issues())

  rows <- which(pair %in% wrong$pair)
  index <- match(pair[rows], wrong$pair)   # outside wrong[...]: there 'pair' would mean its column
  w <- wrong[index]
  typo <- startsWith(w$verdict, "typo:")
  new_issues(dataset, name, rows, field = "measurementType",
             value = sprintf("'%s' with '%s'", w$text, w$label),
             message = ifelse(typo, "measurementType looks like a misspelling of a word in the NERC label",
                              "measurementType has no word in common with the NERC label of measurementTypeID"),
             suggestion = ifelse(typo, sub("^typo:", "", w$verdict), NA_character_))
}

label_stop_words <- c("the", "and", "per", "unit", "units", "specified", "elsewhere", "biological", "entity",
                      "from", "with", "for", "not")

# The words of a text, lower case: "LifeStage" -> "life", "stage". Short and generic words are left out.
text_words <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", x, perl = TRUE)
  words <- tolower(unlist(strsplit(x, "[^A-Za-z]+")))
  unique(words[nchar(words) >= 3 & !words %in% label_stop_words])
}

# "same", "typo:<the word in the label>" or "different".
compare_type_text <- function(text, label, alt) {
  given <- text_words(text)
  known <- text_words(c(label, split_joined(alt)))
  if (!length(given) || !length(known) || any(given %in% known)) return("same")
  long <- given[nchar(given) >= 5]
  if (length(long)) {
    distances <- utils::adist(long, known)
    best <- which(distances == min(distances), arr.ind = TRUE)[1, ]
    if (distances[best[1], best[2]] <= 2) return(paste0("typo:", known[best[2]]))
  }
  "different"
}
