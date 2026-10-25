# =============================================================================
# Checks, group "other"
#
#   dynamicproperties_json    individualcount_valid    organismquantity_with_type
# =============================================================================

# -----------------------------------------------------------------------------
# CHECK  dynamicproperties_json
# What:  if dynamicProperties is filled, it is valid JSON.
# -----------------------------------------------------------------------------
check_dynamicproperties_json <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "dynamicProperties")
  if (!length(tables)) skip_check("no dynamicProperties column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$dynamicProperties
    rows <- rows_where(values, function(v) {
      !vapply(v, function(x) validUTF8(x) && isTRUE(jsonlite::validate(x)), logical(1)) & !is_placeholder(v)
    })
    new_issues(dataset, table_name, rows, field = "dynamicProperties", value = shorten(values[rows]),
               message = "Not valid JSON",
               suggestion = 'write as {"key":"value", "key2":"value2"}')
  }))
}

# -----------------------------------------------------------------------------
# CHECK  individualcount_valid
# What:  individualCount is a whole number of 0 or more.
# -----------------------------------------------------------------------------
check_individualcount_valid <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "individualCount")
  if (!length(tables)) skip_check("no individualCount column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$individualCount
    rows <- rows_where(values, function(v) !is_whole_number(trimws(v)) & !is_placeholder(v) | as_number(v) < 0)
    suggestion <- ifelse(grepl("^\\s*\\d+\\.0+\\s*$", values[rows]), sub("\\.0+\\s*$", "", trimws(values[rows])), NA_character_)
    new_issues(dataset, table_name, rows, field = "individualCount", value = values[rows],
               message = "Not a whole number of 0 or more", suggestion = suggestion)
  }))
}

# -----------------------------------------------------------------------------
# CHECK  organismquantity_with_type
# What:  organismQuantity and organismQuantityType are both filled or both empty.
# -----------------------------------------------------------------------------
check_organismquantity_with_type <- function(dataset, settings) {
  occurrences <- require_table(dataset, "occurrence")
  if (!any(c("organismQuantity", "organismQuantityType") %in% names(occurrences))) {
    skip_check("no organismQuantity or organismQuantityType column")
  }
  quantity <- !is_empty_value(column_or_na(occurrences, "organismQuantity"))
  type <- !is_empty_value(column_or_na(occurrences, "organismQuantityType"))
  combine_issues(list(
    new_issues(dataset, "occurrence", which(quantity & !type), field = "organismQuantityType",
               value = column_or_na(occurrences, "organismQuantity")[quantity & !type],
               message = "organismQuantity without organismQuantityType"),
    new_issues(dataset, "occurrence", which(type & !quantity), field = "organismQuantity",
               value = column_or_na(occurrences, "organismQuantityType")[type & !quantity],
               message = "organismQuantityType without organismQuantity")))
}
