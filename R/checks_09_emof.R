# =============================================================================
# Checks, group "emof": measurements and facts (the offline ones)
#
#   measurementtypeid_filled      measurementunitid_filled
#   value_has_id_or_unit          measurementvalueid_consistent
#   nvs_uri_format                unit_consistent_per_type
#   value_numeric_when_unit       zero_quantity_with_presence
#   positive_quantity_with_absence sampling_effort_present
#   sampling_instrument_present   individualcount_also_in_emof
#   organismquantity_use          emof_type_has_dwc_field
#
# NERC terms are recognised by their code (OCOUNT01, NMSPINST, ...), taken from
# the URI whatever its form. Code lists: inst/reference/*.csv.
# Checks against the NERC vocabulary itself (does the term exist, is it
# deprecated, does the unit fit the type) follow in milestone 4b.
# =============================================================================

# The eMoF table (or old MoF) or skip the check.
require_emof <- function(dataset) {
  name <- emof_table_name(dataset)
  if (is.na(name)) skip_check("no eMoF table")
  name
}

# Empty or only a placeholder ("NA").
is_empty_value <- function(x) is.na(x) | is_placeholder(x)

# Values of a column, or NA for every row when the column is missing.
column_or_na <- function(data, field) {
  if (field %in% names(data)) data[[field]] else rep(NA_character_, nrow(data))
}

# The code of a NERC term: "http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/" -> "OCOUNT01".
nerc_code <- function(uri) {
  per_value(uri, function(u) {
    code <- sub("^.*/collection/[^/]+/current/([^/]+)/?\\s*$", "\\1", trimws(u))
    ifelse(is.na(u) | code == trimws(u), NA_character_, code)
  })
}

# The collection of a NERC term: "... /collection/P01/current/..." -> "P01".
nerc_collection <- function(uri) {
  per_value(uri, function(u) {
    collection <- sub("^.*/collection/([^/]+)/.*$", "\\1", u)
    ifelse(is.na(u) | collection == u, NA_character_, collection)
  })
}

# TRUE where the term's code (or collection) is in the list of codes.
in_code_list <- function(uri, codes) {
  nerc_code(uri) %chin% codes | nerc_collection(uri) %chin% codes
}

# A real unit is given: not empty and not 'not applicable' (P06 XXXX), which is
# the correct unit for text values such as sex or life stage.
has_real_unit <- function(data) {
  unit <- column_or_na(data, "measurementUnit")
  unit_id <- column_or_na(data, "measurementUnitID")
  real_text <- !is_empty_value(unit) & !per_value(unit, function(u) grepl("(?i)^\\s*not applicable\\s*$", u, perl = TRUE))
  real_id <- !is_empty_value(unit_id) & !(nerc_code(unit_id) %in% "XXXX")
  (real_text | real_id) & !(nerc_code(unit_id) %in% "XXXX")
}

# TRUE for eMoF rows that give a length, width or weight, recognised by the
# NERC code of measurementTypeID or by a word in measurementType
# ("Dorsal_mantle_length" counts: _ is read as a space).
# List: inst/reference/size_types.csv (column 'kind' = length / width / weight)
is_size_measurement <- function(data, kinds = c("length", "width", "weight")) {
  size_types <- reference_table("size_types.csv")
  size_types <- size_types$code_or_word[size_types$kind %in% kinds]
  size_words <- paste0("(?i)\\b(", paste(size_types[!grepl("\\d", size_types)], collapse = "|"), ")\\b")
  nerc_code(column_or_na(data, "measurementTypeID")) %chin% size_types |
    per_value(column_or_na(data, "measurementType"), function(t) grepl(size_words, gsub("_", " ", t, fixed = TRUE), perl = TRUE))
}

# The most frequent value (NA when there is none).
most_common <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
}

# A key for "the same measurement type": the type ID when filled, else the type text.
type_key <- function(data) {
  type_id <- column_or_na(data, "measurementTypeID")
  fifelse(is_empty_value(type_id), lower_trim(column_or_na(data, "measurementType")), type_id)
}

# -----------------------------------------------------------------------------
# CHECK  measurementtypeid_filled
# What:  every measurementType has a measurementTypeID.
# Why:   IDs make measurements comparable across datasets; free text is not.
# -----------------------------------------------------------------------------
check_measurementtypeid_filled <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementType")
  rows <- which(!is_empty_value(data$measurementType) & is_empty_value(column_or_na(data, "measurementTypeID")))
  new_issues(dataset, name, rows, field = "measurementTypeID", value = data$measurementType[rows],
             message = "measurementType without measurementTypeID",
             suggestion = "look up the NERC P01 term for this measurement type")
}

# -----------------------------------------------------------------------------
# CHECK  measurementunitid_filled
# What:  every filled measurementUnit has a measurementUnitID (NERC P06).
# -----------------------------------------------------------------------------
check_measurementunitid_filled <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementUnit")
  rows <- which(!is_empty_value(data$measurementUnit) & is_empty_value(column_or_na(data, "measurementUnitID")))
  new_issues(dataset, name, rows, field = "measurementUnitID", value = data$measurementUnit[rows],
             message = "measurementUnit without measurementUnitID",
             suggestion = "look up the NERC P06 term for this unit")
}

# -----------------------------------------------------------------------------
# CHECK  value_has_id_or_unit
# What:  text values ('adult', 'grab') need a measurementValueID; numbers need a unit.
# -----------------------------------------------------------------------------
check_value_has_id_or_unit <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementValue")
  value <- data$measurementValue
  numeric <- !is.na(as_number(value))
  text_without_id <- which(!is_empty_value(value) & !numeric & is_empty_value(column_or_na(data, "measurementValueID")))
  number_without_unit <- which(numeric & is_empty_value(column_or_na(data, "measurementUnit")) &
                               is_empty_value(column_or_na(data, "measurementUnitID")))
  combine_issues(list(
    new_issues(dataset, name, text_without_id, field = "measurementValueID", value = value[text_without_id],
               message = "Text value without measurementValueID",
               suggestion = "look up the NERC term for this value"),
    new_issues(dataset, name, number_without_unit, field = "measurementUnit",
               value = column_or_na(data, "measurementType")[number_without_unit],
               message = "Number without a unit (measurementUnit and measurementUnitID empty)")))
}

# -----------------------------------------------------------------------------
# CHECK  measurementvalueid_consistent
# What:  within a measurement type, the same measurementValue always has the
#        same measurementValueID, and the same measurementValueID the same
#        measurementValue (one-to-one). Measured numbers have no valueID.
# Why:   catches 'adult' with an ID but 'juvenile' without, 'Adult' and 'adult'
#        with one ID, and an ID put on all numbers of a type.
# -----------------------------------------------------------------------------
check_measurementvalueid_consistent <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, c("measurementValue", "measurementValueID"))
  types <- type_key(data)
  is_number <- !is.na(as_number(data$measurementValue))
  values <- data.table(row = seq_len(nrow(data)), type = types, value = data$measurementValue,
                       value_id = ifelse(is_empty_value(data$measurementValueID), NA_character_, data$measurementValueID))
  numbers <- values[is_number & !is.na(value_id)]
  values <- values[!is.na(value) & !is_number]
  values[, `:=`(n_ids = uniqueN(value_id[!is.na(value_id)]), some_without = anyNA(value_id),
                usual_id = most_common(value_id)), by = .(type, value)]
  without_id <- values[n_ids >= 1 & some_without & is.na(value_id)]
  # One valueID for different numbers of the same type: the ID is no value.
  numbers[, `:=`(n_values = uniqueN(value), examples = paste(utils::head(unique(value), 3), collapse = ", "),
                 first_row = row[1]), by = .(type, value_id)]
  numbers <- numbers[n_values > 1]
  number_types <- type_text(data[numbers$first_row])
  # Text values: one valueID per value and one value per valueID, within a type.
  text_values <- one_to_one_issues(dataset, name, "measurementValue", "measurementValueID", within = types, only = !is_number,
                                   within_text = function(rows) type_text(data[rows]))
  # The numbers with one valueID, in the table of combinations: one row per valueID and type.
  groups <- !duplicated(numbers[, .(type, value_id)])
  if (any(groups)) {
    remember_pair(dataset, name, "measurementValue", "measurementValueID", add = TRUE, data.table(
      conflict = sprintf("within %s: measurementValueID '%s' is used for %s different numbers", number_types[groups],
                         numbers$value_id[groups], prettyNum(numbers$n_values[groups], big.mark = ",")),
      field = "measurementValueID", value = numbers$value_id[groups], other_field = "measurementValue",
      other_value = sprintf("%s different numbers (%s, ...)", prettyNum(numbers$n_values[groups], big.mark = ","), numbers$examples[groups]),
      records = numbers[, .N, by = .(type, value_id)]$N, less_common = TRUE, within_group = number_types[groups]))
  }
  combine_issues(list(
    new_issues(dataset, name, without_id$row, field = "measurementValueID", value = without_id$value,
               message = "The same value has a measurementValueID in other records but not here",
               suggestion = without_id$usual_id, severity_hint = "warning"),
    text_values,
    new_issues(dataset, name, numbers$row, field = "measurementValueID; measurementValue", value = numbers$value_id,
               message = "The same measurementValueID is used for different measured numbers",
               suggestion = sprintf(paste("within %s: used for %s different numbers (%s, ...). A measurementValueID stands for one",
                                          "value from a vocabulary (e.g. 'adult'): leave it empty for measured numbers, or move",
                                          "the term to measurementTypeID or measurementUnitID"),
                                    number_types, prettyNum(numbers$n_values, big.mark = ","), numbers$examples))))
}

# -----------------------------------------------------------------------------
# CHECK  nvs_uri_format
# What:  NERC URIs have the form http://vocab.nerc.ac.uk/collection/<C>/current/<code>/
# Why:   badly formed URIs do not resolve and are not recognised by EurOBIS.
# -----------------------------------------------------------------------------
check_nvs_uri_format <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- dataset$tables[[name]]
  fields <- intersect(c("measurementTypeID", "measurementUnitID", "measurementValueID"), names(data))
  if (!length(fields)) skip_check("no measurementTypeID, measurementUnitID or measurementValueID column")
  canonical <- "^http://vocab\\.nerc\\.ac\\.uk/collection/[A-Z0-9]+/current/[A-Za-z0-9_]+/$"
  combine_issues(lapply(fields, function(field) {
    values <- data[[field]]
    found <- find_problem_values(values, function(v) {
      nerc <- grepl("(?i)nerc\\.ac\\.uk|vocab\\.nerc", v, perl = TRUE)
      ifelse(!nerc | grepl(canonical, v), NA_character_,
      ifelse(grepl("\\s", v), "contains spaces",
      ifelse(grepl("^https://", v), "uses https instead of http",
      ifelse(!grepl("/current/", v, fixed = TRUE), "lacks '/current/'",
      ifelse(!grepl("/$", v), "lacks the trailing '/'", "is not in the standard form")))))
    })
    if (!nrow(found)) return(empty_issues())
    uri <- values[found$row]
    code <- sub("^.*/([^/]+)/?\\s*$", "\\1", trimws(uri))
    collection <- sub("^.*/collection/([^/]+)/.*$", "\\1", uri)
    fixable <- grepl("/collection/", uri)
    new_issues(dataset, name, found$row, field = field, value = uri,
               message = paste("The NERC URI", found$message),
               suggestion = ifelse(fixable, sprintf("http://vocab.nerc.ac.uk/collection/%s/current/%s/", collection, code), NA_character_))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  unit_consistent_per_type
# What:  the same measurement type uses one unit throughout the dataset.
# How:   records with another unit than the most used one are reported.
# -----------------------------------------------------------------------------
check_unit_consistent_per_type <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- dataset$tables[[name]]
  unit_id <- column_or_na(data, "measurementUnitID")
  unit <- fifelse(is_empty_value(unit_id), per_value(column_or_na(data, "measurementUnit"), trimws), unit_id)
  units <- data.table(row = seq_len(nrow(data)), type = type_key(data), unit = fifelse(is_empty_value(unit), NA_character_, unit))
  units <- units[!is.na(type) & !is.na(unit)]
  units[, `:=`(n_units = uniqueN(unit), usual = most_common(unit)), by = type]
  other <- units[n_units > 1 & unit != usual]
  new_issues(dataset, name, other$row, field = "measurementUnit", value = other$unit,
             message = "The same measurement type is given in different units in this dataset",
             suggestion = paste("most used unit:", other$usual))
}

# -----------------------------------------------------------------------------
# CHECK  value_numeric_when_unit
# What:  when a unit is given, measurementValue is a number.
# -----------------------------------------------------------------------------
check_value_numeric_when_unit <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementValue")
  has_unit <- has_real_unit(data)
  value <- data$measurementValue
  rows <- which(has_unit & !is_empty_value(value) & is.na(as_number(value)))
  new_issues(dataset, name, rows, field = "measurementValue", value = value[rows],
             message = "A unit is given, but the value is not a number",
             suggestion = number_without_unit(value[rows]))
}

# -----------------------------------------------------------------------------
# CHECK  zero_quantity_with_presence / positive_quantity_with_absence
# What:  'present' with a quantity of 0, or 'absent' with a quantity > 0
#        (eMoF quantity terms, individualCount, organismQuantity).
# List:  inst/reference/nerc_quantity_terms.csv
# -----------------------------------------------------------------------------
check_zero_quantity_with_presence <- function(dataset, settings) {
  quantity_status_issues(dataset, status = c("present", "detected"), problem = function(x) x == 0,
                         message = "occurrenceStatus is 'present', but the quantity is 0")
}

check_positive_quantity_with_absence <- function(dataset, settings) {
  quantity_status_issues(dataset, status = c("absent", "notdetected"), problem = function(x) x > 0,
                         message = "occurrenceStatus is 'absent', but the quantity is more than 0")
}

quantity_status_issues <- function(dataset, status, problem, message) {
  occurrences <- require_fields(dataset, "occurrence", "occurrenceStatus")
  matches_status <- lower_trim(occurrences$occurrenceStatus) %in% status
  issue_list <- list()

  for (field in intersect(c("individualCount", "organismQuantity"), names(occurrences))) {
    quantity <- as_number(occurrences[[field]])
    rows <- which(matches_status & !is.na(quantity) & problem(quantity))
    issue_list[[field]] <- new_issues(dataset, "occurrence", rows, field = field, value = occurrences[[field]][rows],
                                      message = message)
  }

  name <- emof_table_name(dataset)
  if (!is.na(name) && "measurementTypeID" %in% names(dataset$tables[[name]])) {
    emof <- dataset$tables[[name]]
    quantity_codes <- reference_table("nerc_quantity_terms.csv")$code
    occurrence_row <- emof_occurrence_row(dataset)
    quantity <- as_number(emof$measurementValue)
    rows <- which(in_code_list(emof$measurementTypeID, quantity_codes) & !is.na(occurrence_row) &
                  matches_status[occurrence_row] & !is.na(quantity) & problem(quantity))
    issue_list$emof <- new_issues(dataset, name, rows, field = "measurementValue",
                                  value = paste0(column_or_na(emof, "measurementType")[rows], " = ", emof$measurementValue[rows]),
                                  message = message)
  }
  if (!length(issue_list) && (is.na(name) || !"measurementTypeID" %in% names(dataset$tables[[name]]))) {
    skip_check("no quantities (individualCount, organismQuantity or eMoF quantity terms)")
  }
  combine_issues(issue_list)
}

# -----------------------------------------------------------------------------
# CHECK  sampling_effort_present
# What:  the dataset describes sampling effort: an effort term in the eMoF, or
#        sampleSizeValue/Unit, samplingEffort or samplingProtocol.
# List:  inst/reference/sampling_effort_terms.csv
# -----------------------------------------------------------------------------
check_sampling_effort_present <- function(dataset, settings) {
  places <- character()
  name <- emof_table_name(dataset)
  if (!is.na(name) && "measurementTypeID" %in% names(dataset$tables[[name]])) {
    effort_codes <- reference_table("sampling_effort_terms.csv")$code
    if (any(in_code_list(dataset$tables[[name]]$measurementTypeID, effort_codes))) places <- c(places, "eMoF effort term")
  }
  for (field in c("sampleSizeValue", "samplingEffort", "samplingProtocol")) {
    for (table_name in tables_with_fields(dataset, field)) {
      if (any(!is_empty_value(dataset$tables[[table_name]][[field]]))) places <- c(places, field)
    }
  }
  if (length(places)) return(empty_issues())
  new_issues(dataset, if (is.na(name)) dataset$core else name, field = "sampling effort",
             message = "No sampling effort described (no eMoF effort term, sampleSizeValue, samplingEffort or samplingProtocol)",
             suggestion = "add e.g. the sampled area or volume as eMoF (P01 terms linked to P02 SAMP)")
}

# -----------------------------------------------------------------------------
# CHECK  sampling_instrument_present
# What:  the eMoF contains a sampling instrument term (NMSPINST, ideally with an L22 value).
# List:  inst/reference/sampling_instrument_terms.csv
# -----------------------------------------------------------------------------
check_sampling_instrument_present <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- dataset$tables[[name]]
  codes <- reference_table("sampling_instrument_terms.csv")$code
  found <- in_code_list(column_or_na(data, "measurementTypeID"), codes) | in_code_list(column_or_na(data, "measurementValueID"), codes) |
           per_value(column_or_na(data, "measurementType"),
                     function(t) grepl("(?i)instrument|gear|sampler|sampling device", t, perl = TRUE))
  if (any(found)) return(empty_issues())
  new_issues(dataset, name, field = "measurementTypeID",
             message = "No sampling instrument described in the eMoF",
             suggestion = "add NMSPINST (name of sampling instrument) with an L22 term as measurementValueID")
}

# -----------------------------------------------------------------------------
# CHECK  individualcount_also_in_emof
# What:  an occurrence does not have both individualCount and an eMoF count
#        (OCOUNT01) with different values.
# -----------------------------------------------------------------------------
check_individualcount_also_in_emof <- function(dataset, settings) {
  occurrences <- require_fields(dataset, "occurrence", "individualCount")
  name <- require_emof(dataset)
  emof <- require_fields(dataset, name, c("measurementTypeID", "measurementValue"))
  occurrence_row <- emof_occurrence_row(dataset)
  counts <- which(nerc_code(emof$measurementTypeID) == "OCOUNT01" & !is.na(occurrence_row))
  emof_count <- data.table(row = occurrence_row[counts], count = as_number(emof$measurementValue[counts]))[!is.na(count)]
  emof_count <- emof_count[, .(count = count[1]), by = row]
  individual <- as_number(occurrences$individualCount[emof_count$row])
  differs <- !is.na(individual) & individual != emof_count$count
  rows <- emof_count$row[differs]
  new_issues(dataset, "occurrence", rows, field = "individualCount",
             value = sprintf("individualCount %s, eMoF count %s", occurrences$individualCount[rows], emof_count$count[differs]),
             message = "individualCount and the eMoF count (OCOUNT01) of the same occurrence differ")
}

# -----------------------------------------------------------------------------
# CHECK  organismquantity_use
# What:  non-DNA datasets: organismQuantity should be given as eMoF; DNA datasets:
#        organismQuantityType must be 'DNA sequence reads' (viewer decisions 2026-10-13),
#        and so must sampleSizeUnit of the occurrences (as in BioCheck).
# -----------------------------------------------------------------------------
check_organismquantity_use <- function(dataset, settings) {
  occurrences <- require_table(dataset, "occurrence")
  if (is_dna_dataset(dataset)) return(dna_quantity_issues(dataset, occurrences))
  if (!"organismQuantity" %in% names(occurrences)) skip_check("no organismQuantity column in the occurrence table")
  filled <- !is_empty_value(occurrences$organismQuantity)
  rows <- which(filled)
  new_issues(dataset, "occurrence", rows, field = "organismQuantity",
             value = paste(occurrences$organismQuantity[rows], column_or_na(occurrences, "organismQuantityType")[rows]),
             message = "EMODnet stores quantities of non-DNA data in the eMoF",
             suggestion = "give it as an eMoF measurement (e.g. abundance, OCOUNT01 or SDBIOL terms)")
}

# DNA datasets: organismQuantityType and sampleSizeUnit are 'DNA sequence reads'.
dna_quantity_issues <- function(dataset, occurrences) {
  if (!any(c("organismQuantity", "sampleSizeUnit") %in% names(occurrences))) {
    skip_check("no organismQuantity or sampleSizeUnit column in the occurrence table")
  }
  type <- column_or_na(occurrences, "organismQuantityType")
  with_quantity <- which(!is_empty_value(column_or_na(occurrences, "organismQuantity")) & lower_trim(type) != "dna sequence reads")
  unit <- column_or_na(occurrences, "sampleSizeUnit")
  other_unit <- which(!is_empty_value(unit) & lower_trim(unit) != "dna sequence reads")
  combine_issues(list(
    new_issues(dataset, "occurrence", with_quantity, field = "organismQuantityType", value = type[with_quantity],
               message = "In DNA datasets EMODnet keeps organismQuantity only for DNA sequence reads",
               suggestion = "DNA sequence reads"),
    new_issues(dataset, "occurrence", other_unit, field = "sampleSizeUnit", value = unit[other_unit],
               message = "In DNA datasets sampleSizeUnit is 'DNA sequence reads' (sampleSizeValue: the total reads of the sample)",
               suggestion = "DNA sequence reads")))
}

# -----------------------------------------------------------------------------
# CHECK  emof_type_has_dwc_field
# What:  eMoF types that have their own Darwin Core field should use that field.
# List:  inst/reference/p01_to_dwc.csv
# -----------------------------------------------------------------------------
check_emof_type_has_dwc_field <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementTypeID")
  to_dwc <- reference_table("p01_to_dwc.csv")
  code <- nerc_code(data$measurementTypeID)
  rows <- which(code %chin% to_dwc$code)
  new_issues(dataset, name, rows, field = "measurementTypeID", value = column_or_na(data, "measurementType")[rows],
             message = "This measurement has its own Darwin Core field",
             suggestion = paste("use the field", to_dwc$dwc_field[match(code[rows], to_dwc$code)]))
}
