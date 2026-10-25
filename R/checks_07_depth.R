# =============================================================================
# Checks, group "depth": are depths and elevations numbers, in order, possible?
#
#   depth_numeric    depth_min_le_max    depth_in_range    elevation_valid
#   depth_vs_bathymetry  (online: OBIS xylookup, R/33_lookup_xy.R)
# =============================================================================

depth_fields <- c("minimumDepthInMeters", "maximumDepthInMeters")

# Run make_issues(table_name, field, values) for every depth column.
for_each_depth <- function(dataset, make_issues, fields = depth_fields) {
  tables <- unique(unlist(lapply(fields, function(f) tables_with_fields(dataset, f))))
  if (!length(tables)) skip_check(sprintf("no %s column", paste(fields, collapse = " / ")))
  issue_list <- list()
  for (table_name in tables) {
    for (field in intersect(fields, names(dataset$tables[[table_name]]))) {
      issue_list[[length(issue_list) + 1]] <- make_issues(table_name, field, dataset$tables[[table_name]][[field]])
    }
  }
  combine_issues(issue_list)
}

# -----------------------------------------------------------------------------
# CHECK  depth_numeric
# What:  minimum/maximumDepthInMeters are numbers; hints for units ('10 m'),
#        ranges ('10-20') and decimal commas.
# -----------------------------------------------------------------------------
check_depth_numeric <- function(dataset, settings) {
  for_each_depth(dataset, function(table_name, field, values) {
    rows <- rows_where(values, function(v) !is_decimal_number(trimws(v)) & !is_placeholder(v))
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = "Not a number of metres", suggestion = suggest_depth(values[rows]))
  })
}

# "10 m" -> "10"; "10-20" -> "minimumDepthInMeters 10, maximumDepthInMeters 20"; "10,5" -> "10.5"
suggest_depth <- function(x) {
  range <- regmatches(x, regexec("^\\s*(\\d+(?:[.,]\\d+)?)\\s*(?:-|to)\\s*(\\d+(?:[.,]\\d+)?)\\s*(?:m|meters?|metres?)?\\s*$", x, perl = TRUE))
  ifelse(lengths(range) == 3,
         vapply(range, function(m) if (length(m) == 3) sprintf("minimumDepthInMeters %s, maximumDepthInMeters %s", sub(",", ".", m[2]), sub(",", ".", m[3])) else NA_character_, ""),
         number_without_unit(x))
}

# -----------------------------------------------------------------------------
# CHECK  depth_min_le_max
# What:  minimumDepthInMeters is not greater than maximumDepthInMeters.
# -----------------------------------------------------------------------------
check_depth_min_le_max <- function(dataset, settings) {
  min_max_issues(dataset, depth_fields, "The minimum depth is greater than the maximum depth")
}

min_max_issues <- function(dataset, fields, message) {
  tables <- tables_with_fields(dataset, fields)
  if (!length(tables)) skip_check(sprintf("no table with both %s", paste(fields, collapse = " and ")))
  combine_issues(lapply(tables, function(table_name) {
    data <- dataset$tables[[table_name]]
    minimum <- as_number(data[[fields[1]]])
    maximum <- as_number(data[[fields[2]]])
    rows <- which(minimum > maximum)
    new_issues(dataset, table_name, rows, field = paste(fields, collapse = "; "),
               value = paste(data[[fields[1]]][rows], data[[fields[2]]][rows], sep = "; "),
               message = message, suggestion = "swap the two values?")
  }))
}

# -----------------------------------------------------------------------------
# CHECK  depth_in_range
# What:  depth is not negative and not deeper than max_m (default 11,000 m).
# Settings: max_m (11000)
# -----------------------------------------------------------------------------
check_depth_in_range <- function(dataset, settings) {
  max_m <- setting(settings, "max_m", 11000)
  for_each_depth(dataset, function(table_name, field, values) {
    depth <- as_number(values)
    rows <- which(depth < 0 | depth > max_m)
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = ifelse(depth[rows] < 0, "Negative depth (above sea level): use elevation instead, or a positive depth",
                                sprintf("Deeper than %s m", format(max_m, big.mark = ","))))
  })
}

# -----------------------------------------------------------------------------
# CHECK  elevation_valid
# What:  minimum/maximumElevationInMeters are numbers and minimum <= maximum.
# -----------------------------------------------------------------------------
check_elevation_valid <- function(dataset, settings) {
  fields <- c("minimumElevationInMeters", "maximumElevationInMeters")
  not_numbers <- for_each_depth(dataset, fields = fields, function(table_name, field, values) {
    rows <- rows_where(values, function(v) !is_decimal_number(trimws(v)) & !is_placeholder(v))
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = "Not a number of metres", suggestion = number_without_unit(values[rows]))
  })
  order <- tryCatch(min_max_issues(dataset, fields, "The minimum elevation is greater than the maximum elevation"),
                    qc_not_applicable = function(condition) empty_issues())
  combine_issues(list(not_numbers, order))
}

# -----------------------------------------------------------------------------
# CHECK  depth_vs_bathymetry
# What:  the depth is not deeper than the sea floor at the position (own or
#        inherited) plus margin_m. The depth is maximumDepthInMeters, or
#        minimumDepthInMeters where no maximum is given (as obistools does).
# How:   sea floor depth from OBIS xylookup (EMODnet Bathymetry / GEBCO grid).
#        Needs the internet (or the cache): no offline fallback.
# Settings: margin_m (150)
# -----------------------------------------------------------------------------
check_depth_vs_bathymetry <- function(dataset, settings) {
  margin_m <- setting(settings, "margin_m", 150)
  tables <- intersect(c("event", "occurrence"), unique(c(tables_with_fields(dataset, "maximumDepthInMeters"),
                                                         tables_with_fields(dataset, "minimumDepthInMeters"))))
  if (!length(tables)) skip_check("no maximumDepthInMeters or minimumDepthInMeters column in the event or occurrence table")

  combine_issues(lapply(tables, function(table_name) {
    if (!field_available(dataset, table_name, "decimalLatitude") || !field_available(dataset, table_name, "decimalLongitude")) return(NULL)
    data <- dataset$tables[[table_name]]
    written <- column_or_na(data, "maximumDepthInMeters")
    depth <- as_number(written)
    field <- rep("maximumDepthInMeters", nrow(data))
    use_minimum <- which(is.na(depth) & !is.na(as_number(column_or_na(data, "minimumDepthInMeters"))))
    written[use_minimum] <- data$minimumDepthInMeters[use_minimum]
    depth[use_minimum] <- as_number(written[use_minimum])
    field[use_minimum] <- "minimumDepthInMeters"

    measured <- which(!is.na(depth))
    if (!length(measured)) return(NULL)
    lat <- as_number(inherited_value(dataset, table_name, "decimalLatitude")$value[measured])
    lon <- as_number(inherited_value(dataset, table_name, "decimalLongitude")$value[measured])
    sea_floor <- xy_for_records(dataset, lon, lat)$bathymetry
    too_deep <- which(!is.na(sea_floor) & depth[measured] > pmax(sea_floor, 0) + margin_m)
    rows <- measured[too_deep]
    new_issues(dataset, table_name, rows, field = field[rows],
               value = sprintf("%s m; sea floor about %s m", written[rows], round(sea_floor[too_deep])),
               message = sprintf("Deeper than the sea floor at this position plus %s m (OBIS bathymetry)", margin_m))
  }))
}
