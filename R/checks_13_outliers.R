# =============================================================================
# Checks, group "outliers": cheap, explained, never blocking
#
#   outlier_location_dataset    outlier_date_dataset     outlier_depth_dataset
#   outlier_measurement_value   outlier_size_per_species outlier_species_location
#
# Two methods (docs/design.md, section 4.9):
#   95% range + margin    the range of the middle 95% of the values, widened by
#                         half its width on both sides (with a minimum margin);
#                         only values outside that are flagged, NOT simply the
#                         outer 5%. Used for positions, dates, depths, species
#                         positions. Counts DIFFERENT values (positions), so one
#                         station with many records does not decide the range.
#   3 x IQR, log scale    Tukey's 'far out' rule on log10 for positive, skewed
#                         quantities (counts, weights, lengths). Used for
#                         measurement values and sizes per species.
# Every message gives the numbers behind it. Settings: see the rules workbook.
# =============================================================================

# The accepted band around the middle 'inner_percent' of x.
band_of <- function(x, inner_percent, widen_factor, min_margin) {
  tail <- (1 - inner_percent / 100) / 2
  q <- stats::quantile(x, c(tail, 1 - tail), na.rm = TRUE, names = FALSE)
  margin <- max(widen_factor * (q[2] - q[1]), min_margin)
  list(low = q[1], high = q[2], lower = q[1] - margin, upper = q[2] + margin)
}

# TRUE for values far outside the middle 50% (Tukey), on log10 when all values are positive.
far_out <- function(x, iqr_factor) {
  scale <- if (all(x > 0)) log10(x) else x
  q <- stats::quantile(scale, c(0.25, 0.75), names = FALSE)
  iqr <- q[2] - q[1]
  if (iqr == 0) return(rep(FALSE, length(x)))
  scale < q[1] - iqr_factor * iqr | scale > q[2] + iqr_factor * iqr
}

# -----------------------------------------------------------------------------
# CHECK  outlier_location_dataset
# What:  positions far outside the area of the middle 95% of sampling positions.
# Settings: inner_percent (95), widen_factor (0.5), min_margin_degrees (1), min_positions (20)
# -----------------------------------------------------------------------------
check_outlier_location_dataset <- function(dataset, settings) {
  inner <- setting(settings, "inner_percent", 95)
  widen <- setting(settings, "widen_factor", 0.5)
  min_margin <- setting(settings, "min_margin_degrees", 1)
  min_positions <- setting(settings, "min_positions", 20)

  issues <- lapply(position_tables(dataset), function(table_name) {
    data <- dataset$tables[[table_name]]
    lat <- as_number(data$decimalLatitude)
    lon <- as_number(data$decimalLongitude)
    valid <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
    positions <- unique(data.table(lat = lat[valid], lon = lon[valid]))
    if (nrow(positions) < min_positions) return(NULL)

    # Across the 180 degree meridian (e.g. the Pacific) longitudes 0-360 give a tighter range.
    lon_360 <- function(x) ifelse(x < 0, x + 360, x)
    width <- function(x) {
      band <- band_of(x, inner, 0, 0)
      band$high - band$low
    }
    lon_for_band <- if (width(lon_360(positions$lon)) < width(positions$lon)) lon_360 else identity

    lat_band <- band_of(positions$lat, inner, widen, min_margin)
    lon_band <- band_of(lon_for_band(positions$lon), inner, widen, min_margin)
    lon_used <- lon_for_band(lon)
    rows <- which(valid & (lat < lat_band$lower | lat > lat_band$upper | lon_used < lon_band$lower | lon_used > lon_band$upper))
    new_issues(dataset, table_name, rows, field = "decimalLatitude; decimalLongitude",
               value = paste(data$decimalLatitude[rows], data$decimalLongitude[rows], sep = "; "),
               message = sprintf("Far from the other sampling positions: %g%% of them lie between latitude %.2f and %.2f and longitude %.2f and %.2f",
                                 inner, lat_band$low, lat_band$high, lon_band$low, lon_band$high))
  })
  if (all(vapply(issues, is.null, logical(1)))) skip_check(sprintf("fewer than %d different positions", min_positions))
  combine_issues(issues)
}

# -----------------------------------------------------------------------------
# CHECK  outlier_date_dataset
# What:  dates far outside the period of the middle 95% of the sampling dates.
# Settings: inner_percent (95), widen_factor (0.5), min_margin_years (2), min_dates (20)
# -----------------------------------------------------------------------------
check_outlier_date_dataset <- function(dataset, settings) {
  inner <- setting(settings, "inner_percent", 95)
  widen <- setting(settings, "widen_factor", 0.5)
  min_margin <- setting(settings, "min_margin_years", 2)
  min_dates <- setting(settings, "min_dates", 20)
  as_year <- function(date) as.numeric(format(date, "%Y")) + (as.numeric(format(date, "%j")) - 1) / 366

  issues <- lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    range <- iso_date_range(values)
    start <- as_year(range$start)
    end <- as_year(range$end)
    distinct <- unique(start[!is.na(start)])
    if (length(distinct) < min_dates) return(NULL)
    band <- band_of(distinct, inner, widen, min_margin)
    rows <- which(!is.na(start) & (start < band$lower | end > band$upper))
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = sprintf("Far from the other dates: %g%% of the sampling dates lie between %d and %d",
                                 inner, floor(band$low), floor(band$high)))
  })
  if (all(vapply(issues, is.null, logical(1)))) skip_check(sprintf("fewer than %d different dates", min_dates))
  combine_issues(issues)
}

# -----------------------------------------------------------------------------
# CHECK  outlier_depth_dataset
# What:  depths far outside the range of the middle 95% of the depths.
# Settings: inner_percent (95), widen_factor (0.5), min_margin_m (50), min_values (20)
# -----------------------------------------------------------------------------
check_outlier_depth_dataset <- function(dataset, settings) {
  inner <- setting(settings, "inner_percent", 95)
  widen <- setting(settings, "widen_factor", 0.5)
  min_margin <- setting(settings, "min_margin_m", 50)
  min_values <- setting(settings, "min_values", 20)
  any_checked <- FALSE
  issues <- for_each_depth(dataset, function(table_name, field, values) {
    depth <- as_number(values)
    distinct <- unique(depth[!is.na(depth)])
    if (length(distinct) < min_values) return(NULL)
    any_checked <<- TRUE
    band <- band_of(distinct, inner, widen, min_margin)
    rows <- which(depth < band$lower | depth > band$upper)
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = sprintf("Far from the other depths: %g%% of them lie between %g and %g m", inner, band$low, band$high))
  })
  if (!any_checked) skip_check(sprintf("fewer than %d different depths", min_values))
  issues
}

# -----------------------------------------------------------------------------
# CHECK  outlier_measurement_value
# What:  per measurement type and unit, values far outside the middle 50%
#        (3 interquartile ranges, log scale for positive values).
# Settings: iqr_factor (3), min_values (20)
# -----------------------------------------------------------------------------
check_outlier_measurement_value <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementValue")
  iqr_factor <- setting(settings, "iqr_factor", 3)
  min_values <- setting(settings, "min_values", 20)
  unit_id <- column_or_na(data, "measurementUnitID")
  values <- data.table(row = seq_len(nrow(data)), type = type_key(data),
                       unit = ifelse(is_empty_value(unit_id), column_or_na(data, "measurementUnit"), unit_id),
                       number = as_number(data$measurementValue))[!is.na(number) & !is.na(type)]
  far_out_rows(dataset, name, values, by = c("type", "unit"), iqr_factor, min_values,
               label = function(type) sprintf("Unusual value for this measurement type (%s)", type))
}

# Flag far-out values per group; the message gives the usual range of the group.
far_out_rows <- function(dataset, table_name, values, by, iqr_factor, min_values, label) {
  values[, n := .N, by = by]
  values <- values[n >= min_values]
  if (!nrow(values)) skip_check(sprintf("no group with at least %d values", min_values))
  values[, `:=`(flagged = far_out(number, iqr_factor),
                usual = sprintf("50%% of the values lie between %g and %g",
                                signif(stats::quantile(number, 0.25, names = FALSE), 3),
                                signif(stats::quantile(number, 0.75, names = FALSE), 3))), by = by]
  flagged <- values[flagged == TRUE]
  new_issues(dataset, table_name, flagged$row, field = "measurementValue",
             value = dataset$tables[[table_name]]$measurementValue[flagged$row],
             message = paste0(label(flagged[[by[1]]]), ": ", flagged$usual))
}

# -----------------------------------------------------------------------------
# CHECK  outlier_size_per_species
# What:  per species and length/weight type and unit, values far from the other
#        values of the same species (3 IQR on a log scale).
# List:  inst/reference/size_types.csv
# Settings: iqr_factor (3), min_values (10)
# -----------------------------------------------------------------------------
check_outlier_size_per_species <- function(dataset, settings) {
  name <- require_emof(dataset)
  data <- require_fields(dataset, name, "measurementValue")
  occurrences <- require_table(dataset, "occurrence")
  iqr_factor <- setting(settings, "iqr_factor", 3)
  min_values <- setting(settings, "min_values", 10)

  occurrence_row <- emof_occurrence_row(dataset)
  # Only the size records go on; the eMoF can have tens of millions of rows.
  rows <- which(is_size_measurement(data) & !is.na(occurrence_row))
  if (!length(rows)) skip_check("no lengths or weights linked to occurrences")
  data <- data[rows]
  species_id <- column_or_na(occurrences, "scientificNameID")
  species <- fifelse(is.na(species_id), column_or_na(occurrences, "scientificName"), species_id)

  unit_id <- column_or_na(data, "measurementUnitID")
  values <- data.table(row = rows, species = species[occurrence_row[rows]], type = type_key(data),
                       unit = fifelse(is_empty_value(unit_id), column_or_na(data, "measurementUnit"), unit_id),
                       number = as_number(data$measurementValue))[!is.na(number) & !is.na(species)]
  if (!nrow(values)) skip_check("no lengths or weights linked to occurrences")
  far_out_rows(dataset, name, values, by = c("species", "type", "unit"), iqr_factor, min_values,
               label = function(species) "Unusual size for this species")
}

# -----------------------------------------------------------------------------
# CHECK  outlier_species_location
# What:  per species, positions far from where the same species was found
#        elsewhere in this dataset.
# Settings: inner_percent (95), widen_factor (0.5), min_positions (10), min_margin_km (100)
# -----------------------------------------------------------------------------
check_outlier_species_location <- function(dataset, settings) {
  occurrences <- require_table(dataset, "occurrence")
  inner <- setting(settings, "inner_percent", 95)
  widen <- setting(settings, "widen_factor", 0.5)
  min_positions <- setting(settings, "min_positions", 10)
  min_margin_km <- setting(settings, "min_margin_km", 100)

  species_id <- column_or_na(occurrences, "scientificNameID")
  species <- ifelse(is.na(species_id), column_or_na(occurrences, "scientificName"), species_id)
  lat <- as_number(inherited_value(dataset, "occurrence", "decimalLatitude")$value)
  lon <- as_number(inherited_value(dataset, "occurrence", "decimalLongitude")$value)
  records <- data.table(row = seq_len(nrow(occurrences)), species = species, lat = lat, lon = lon)[
    !is.na(species) & !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180]
  if (!nrow(records)) skip_check("no occurrences with species and position")

  positions <- unique(records[, .(species, lat, lon)])
  positions[, n := .N, by = species]
  positions <- positions[n >= min_positions]
  if (!nrow(positions)) skip_check(sprintf("no species with at least %d different positions", min_positions))
  positions[, distance := distance_km(lat, lon, stats::median(lat), stats::median(lon)), by = species]
  positions[, limit := band_of(distance, inner, widen, min_margin_km)$upper, by = species]
  far <- positions[distance > limit]
  flagged <- records[far, on = .(species, lat, lon), nomatch = NULL]   # brings the distance along
  new_issues(dataset, "occurrence", flagged$row, field = "decimalLatitude; decimalLongitude",
             value = sprintf("%s; %s (%.0f km from the species' centre)", flagged$lat, flagged$lon, flagged$distance),
             message = "Far from the other records of this species in the dataset")
}

# Great-circle distance in km (haversine).
distance_km <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  a <- sin((lat2 - lat1) * to_rad / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin((lon2 - lon1) * to_rad / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}
