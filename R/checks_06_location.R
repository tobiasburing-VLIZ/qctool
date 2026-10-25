# =============================================================================
# Checks, group "location": are the coordinates numbers, possible, and plausible?
#
#   coordinates_numeric     coordinates_in_range     longitude_0_360
#   coordinates_swapped     coordinates_not_zero     coordinates_precision
#   coordinateuncertainty_valid                      coordinates_within_eml_area
#   points_on_land          marine_taxon_on_land     (online: OBIS xylookup + WoRMS)
#
# Coordinates are checked where they are written (event or occurrence table).
# Land: R/33_lookup_xy.R (offline fallback: the coarse 'maps' coastline).
# =============================================================================

coordinate_fields <- c(decimalLatitude = "latitude", decimalLongitude = "longitude")

# Tables with at least one coordinate column.
coordinate_tables <- function(dataset) {
  tables <- unique(c(tables_with_fields(dataset, "decimalLatitude"), tables_with_fields(dataset, "decimalLongitude")))
  if (!length(tables)) skip_check("no decimalLatitude or decimalLongitude column")
  tables
}

# Tables with both coordinate columns.
position_tables <- function(dataset) {
  tables <- tables_with_fields(dataset, c("decimalLatitude", "decimalLongitude"))
  if (!length(tables)) skip_check("no table with both decimalLatitude and decimalLongitude")
  tables
}

# Run make_issues(table_name, field, values) for every coordinate column.
for_each_coordinate <- function(dataset, make_issues) {
  issue_list <- list()
  for (table_name in coordinate_tables(dataset)) {
    for (field in intersect(names(coordinate_fields), names(dataset$tables[[table_name]]))) {
      issue_list[[length(issue_list) + 1]] <- make_issues(table_name, field, dataset$tables[[table_name]][[field]])
    }
  }
  combine_issues(issue_list)
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_numeric
# What:  decimalLatitude and decimalLongitude are numbers with a dot as decimal
#        separator; hints for decimal commas, degree signs and N/S/E/W letters.
# -----------------------------------------------------------------------------
check_coordinates_numeric <- function(dataset, settings) {
  for_each_coordinate(dataset, function(table_name, field, values) {
    rows <- rows_where(values, function(v) !is_decimal_number(trimws(v)) & !is_placeholder(v))
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = "Not a number in decimal degrees",
               suggestion = suggest_decimal_degrees(values[rows], latitude = field == "decimalLatitude"))
  })
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_in_range
# What:  latitude between -90 and 90, longitude between -180 and 180.
#        Longitudes between 180 and 360 are reported by longitude_0_360 instead.
# -----------------------------------------------------------------------------
check_coordinates_in_range <- function(dataset, settings) {
  for_each_coordinate(dataset, function(table_name, field, values) {
    number <- as_number(values)
    outside <- if (field == "decimalLatitude") abs(number) > 90 else number < -180 | number > 360
    rows <- which(outside)
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = if (field == "decimalLatitude") "Latitude outside -90 to 90" else "Longitude outside -180 to 180")
  })
}

# -----------------------------------------------------------------------------
# CHECK  longitude_0_360
# What:  longitudes between 180 and 360 look like 0-360 notation: suggests longitude - 360.
# Why:   common in satellite (Argos) data.
# -----------------------------------------------------------------------------
check_longitude_0_360 <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "decimalLongitude")
  if (!length(tables)) skip_check("no decimalLongitude column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$decimalLongitude
    number <- as_number(values)
    rows <- which(number > 180 & number <= 360)
    new_issues(dataset, table_name, rows, field = "decimalLongitude", value = values[rows],
               message = "Longitude in 0-360 notation",
               suggestion = format(number[rows] - 360, scientific = FALSE, drop0trailing = TRUE))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_swapped
# What:  latitude impossible but longitude possible as a latitude; or the point
#        lies outside the metadata area while the swapped point lies inside.
# -----------------------------------------------------------------------------
check_coordinates_swapped <- function(dataset, settings) {
  boxes <- eml_boxes(dataset, required = FALSE)
  combine_issues(lapply(position_tables(dataset), function(table_name) {
    data <- dataset$tables[[table_name]]
    lat <- as_number(data$decimalLatitude)
    lon <- as_number(data$decimalLongitude)
    impossible <- !is.na(lat) & !is.na(lon) & abs(lat) > 90 & abs(lon) <= 90
    in_area_swapped <- if (nrow(boxes)) {
      !is.na(lat) & !is.na(lon) & !impossible & !inside_boxes(lat, lon, boxes) & inside_boxes(lon, lat, boxes)
    } else {
      rep(FALSE, nrow(data))
    }
    rows <- which(impossible | in_area_swapped)
    new_issues(dataset, table_name, rows, field = "decimalLatitude; decimalLongitude",
               value = paste(data$decimalLatitude[rows], data$decimalLongitude[rows], sep = "; "),
               message = ifelse(impossible[rows], "Latitude and longitude seem swapped (the latitude is impossible)",
                                "Latitude and longitude seem swapped (swapped, the point lies in the metadata area)"),
               suggestion = paste0("decimalLatitude = ", data$decimalLongitude[rows], ", decimalLongitude = ", data$decimalLatitude[rows]))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_not_zero
# What:  latitude and longitude are not both 0.
# Why:   0,0 is a typical default for missing coordinates (Gulf of Guinea).
# -----------------------------------------------------------------------------
check_coordinates_not_zero <- function(dataset, settings) {
  combine_issues(lapply(position_tables(dataset), function(table_name) {
    data <- dataset$tables[[table_name]]
    rows <- which(as_number(data$decimalLatitude) == 0 & as_number(data$decimalLongitude) == 0)
    new_issues(dataset, table_name, rows, field = "decimalLatitude; decimalLongitude", value = "0; 0",
               message = "Latitude and longitude are both 0: probably a missing position")
  }))
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_precision
# What:  coordinates have at most max_decimals decimals (default 7, about 1 cm).
# Why:   more usually comes from a conversion (43.558590000000002 from Excel).
# Settings: max_decimals (7)
# -----------------------------------------------------------------------------
check_coordinates_precision <- function(dataset, settings) {
  max_decimals <- setting(settings, "max_decimals", 7)
  for_each_coordinate(dataset, function(table_name, field, values) {
    rows <- rows_where(values, function(v) {
      v <- trimws(v)
      is_decimal_number(v) & grepl(".", v, fixed = TRUE) & nchar(sub("^[^.]*\\.", "", v)) > max_decimals
    })
    new_issues(dataset, table_name, rows, field = field, value = values[rows],
               message = sprintf("More than %d decimals: probably a conversion artefact", max_decimals),
               suggestion = format(round(as_number(values[rows]), max_decimals), scientific = FALSE, drop0trailing = TRUE))
  })
}

# -----------------------------------------------------------------------------
# CHECK  coordinateuncertainty_valid
# What:  coordinateUncertaintyInMeters is a number > 0 and <= 20,037,509 m.
# -----------------------------------------------------------------------------
check_coordinateuncertainty_valid <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "coordinateUncertaintyInMeters")
  if (!length(tables)) skip_check("no coordinateUncertaintyInMeters column")
  combine_issues(lapply(tables, function(table_name) {
    values <- dataset$tables[[table_name]]$coordinateUncertaintyInMeters
    number <- as_number(values)
    written <- !is.na(values) & !is_placeholder(values)
    not_number <- which(written & is.na(number))
    impossible <- which(!is.na(number) & (number <= 0 | number > 20037509))
    combine_issues(list(
      new_issues(dataset, table_name, not_number, field = "coordinateUncertaintyInMeters", value = values[not_number],
                 message = "Not a number of metres", suggestion = number_without_unit(values[not_number])),
      new_issues(dataset, table_name, impossible, field = "coordinateUncertaintyInMeters", value = values[impossible],
                 message = "Must be more than 0 and at most 20,037,509 m (half the Earth's circumference)")))
  }))
}

# "100 m" -> "100", "10,5" -> "10.5"; NA when not that simple.
number_without_unit <- function(x) {
  cleaned <- sub(",", ".", trimws(sub("(?i)\\s*(m|meters?|metres?)$", "", trimws(x), perl = TRUE)), fixed = TRUE)
  ifelse(is_decimal_number(cleaned), cleaned, NA_character_)
}

# -----------------------------------------------------------------------------
# CHECK  coordinates_within_eml_area
# What:  points fall inside the geographic coverage (bounding box) of the metadata.
# -----------------------------------------------------------------------------
check_coordinates_within_eml_area <- function(dataset, settings) {
  boxes <- eml_boxes(dataset)
  combine_issues(lapply(position_tables(dataset), function(table_name) {
    data <- dataset$tables[[table_name]]
    lat <- as_number(data$decimalLatitude)
    lon <- as_number(data$decimalLongitude)
    rows <- which(!is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180 & !inside_boxes(lat, lon, boxes))
    new_issues(dataset, table_name, rows, field = "decimalLatitude; decimalLongitude",
               value = paste(data$decimalLatitude[rows], data$decimalLongitude[rows], sep = "; "),
               message = "The point lies outside the geographic coverage of the metadata",
               suggestion = paste("metadata box (W, E, S, N):", paste(sprintf("%s, %s, %s, %s", boxes$west, boxes$east, boxes$south, boxes$north), collapse = "; ")))
  }))
}

# The bounding boxes of the EML geographic coverage.
eml_boxes <- function(dataset, required = TRUE) {
  geographic <- dataset$metadata$geographic
  boxes <- if (is.null(geographic)) data.table() else geographic[!is.na(west) & !is.na(east) & !is.na(north) & !is.na(south)]
  if (required && !nrow(boxes)) skip_check("no bounding box in the metadata")
  boxes
}

# TRUE where the point lies in at least one box. Boxes with west > east cross
# the 180 degree meridian.
inside_boxes <- function(lat, lon, boxes) {
  Reduce(`|`, lapply(seq_len(nrow(boxes)), function(i) {
    b <- boxes[i]
    in_lon <- if (b$west <= b$east) lon >= b$west & lon <= b$east else lon >= b$west | lon <= b$east
    !is.na(lat) & !is.na(lon) & lat >= b$south & lat <= b$north & in_lon
  }), FALSE)
}

# --- Land (OBIS xylookup, offline: coarse coastline) --------------------------------

# -----------------------------------------------------------------------------
# CHECK  points_on_land
# What:  positions on land, further inland than buffer_m from the coast. A flag
#        for a person to judge (estuaries, rivers and lagoons can be right).
# How:   distance to the coast from OBIS xylookup. Offline: the coastline of
#        the 'maps' package, which is coarse - those issues say "approximate".
# Settings: buffer_m (3000)
# -----------------------------------------------------------------------------
check_points_on_land <- function(dataset, settings) {
  buffer_m <- setting(settings, "buffer_m", 3000)
  combine_issues(lapply(position_tables(dataset), function(table_name) {
    data <- dataset$tables[[table_name]]
    land <- land_at(dataset, as_number(data$decimalLongitude), as_number(data$decimalLatitude), buffer_m)
    rows <- which(land$on_land %in% TRUE)
    new_issues(dataset, table_name, rows, field = "decimalLatitude; decimalLongitude",
               value = position_text(data$decimalLatitude[rows], data$decimalLongitude[rows], land$distance_km[rows]),
               message = land_message("Position on land", land, buffer_m))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  marine_taxon_on_land
# What:  occurrences on land (own or inherited position, same buffer as
#        points_on_land) of taxa that WoRMS lists as marine only.
# Offline (approximate coastline) these are warnings instead of errors.
# -----------------------------------------------------------------------------
check_marine_taxon_on_land <- function(dataset, settings) {
  if (!field_available(dataset, "occurrence", "decimalLatitude") || !field_available(dataset, "occurrence", "decimalLongitude")) {
    skip_check("no coordinates for the occurrences")
  }
  buffer_m <- land_buffer(dataset, settings)
  taxa <- occurrence_worms(dataset)
  marine <- marine_only_taxa(dataset, taxa$records)
  candidates <- which(taxa$ids %chin% marine$query)
  if (!length(candidates)) return(empty_issues())

  lat <- inherited_value(dataset, "occurrence", "decimalLatitude")$value[candidates]
  lon <- inherited_value(dataset, "occurrence", "decimalLongitude")$value[candidates]
  land <- land_at(dataset, as_number(lon), as_number(lat), buffer_m)
  hit <- which(land$on_land %in% TRUE)
  rows <- candidates[hit]
  record <- marine[match(taxa$ids[rows], marine$query)]
  approximate <- !identical(attr(land, "source"), "OBIS xylookup")
  new_issues(dataset, "occurrence", rows, field = "decimalLatitude; decimalLongitude",   # the position is the doubt, not the taxon
             value = sprintf("%s at %s", record$scientificname, position_text(lat[hit], lon[hit], land$distance_km[hit])),
             message = land_message("Marine-only taxon on land", land, buffer_m),
             severity_hint = if (approximate) "warning" else NA_character_)
}

# The buffer of this check, else the one of points_on_land, else 3 km.
land_buffer <- function(dataset, settings) {
  own <- setting(settings, "buffer_m", NA)
  if (!is.na(own)) return(own)
  if (!is.null(dataset$rules)) return(setting(settings_for(dataset$rules, "points_on_land"), "buffer_m", 3000))
  3000
}

position_text <- function(lat, lon, distance_km) {
  paste0(lat, ", ", lon, ifelse(is.na(distance_km), "", paste0(" (", distance_km, " km inland)")))
}

land_message <- function(start, land, buffer_m) {
  if (identical(attr(land, "source"), "OBIS xylookup")) {
    sprintf("%s: more than %s km from the coast (OBIS)", start, buffer_m / 1000)
  } else {
    sprintf("%s: more than %s km inland on the offline coastline (approximate)", start, buffer_m / 1000)
  }
}
