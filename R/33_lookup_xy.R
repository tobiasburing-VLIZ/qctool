# =============================================================================
# Lookups by position: OBIS xylookup (distance to the coast, sea floor depth)
#   https://api.obis.org/xylookup - the same service OBIS uses for its own
#   quality flags. shoredistance: metres, negative on land. bathymetry: metres
#   below sea level (EMODnet Bathymetry / GEBCO), negative on land.
#
#   xy_lookup()  shoredistance and bathymetry per position (5,000 per request)
#   land_at()    on land beyond a buffer? Online: xylookup. Offline (or when
#                OBIS does not answer): the coastline of the 'maps' package -
#                coarse, so its issues are marked as approximate.
# Each different position is looked up once (FIFD: 43k positions, not 8M records).
# Common part (cache, offline, retries): 30_lookup.R
# =============================================================================

xylookup_url <- "https://api.obis.org/xylookup"

# "lon;lat" with 5 decimals (about 1 m): the key in the cache.
position_key <- function(lon, lat) sprintf("%.5f;%.5f", lon, lat)

xy_lookup <- function(dataset, lon, lat) {
  lookup_values(dataset, "xylookup", position_key(lon, lat), fetch_xylookup, batch_size = 5000, what = "positions")
}

fetch_xylookup <- function(batches) {
  requests <- lapply(batches, function(keys) {
    parts <- strsplit(keys, ";", fixed = TRUE)
    points <- lapply(parts, as.numeric)
    list(url = xylookup_url, content_type = "application/json",
         body = jsonlite::toJSON(list(points = points, areas = FALSE, grids = TRUE, shoredistance = TRUE),
                                 auto_unbox = TRUE, digits = NA))
  })
  fetch_and_parse(batches, requests, "OBIS xylookup", parse_xylookup)
}

# The answer has one entry per point, in the order of the request.
parse_xylookup <- function(keys, response) {
  answers <- jsonlite::fromJSON(response_text(response, "OBIS xylookup"), simplifyVector = FALSE)
  if (length(answers) != length(keys)) {
    stop(structure(class = c("qc_http_failed", "error", "condition"),
                   list(message = "OBIS xylookup gave an answer of the wrong length", call = NULL)))
  }
  data.table(query = keys, found = TRUE,
             shoredistance = vapply(answers, function(a) as.numeric(json_field(a, "shoredistance", NA_real_)), numeric(1)),
             bathymetry = vapply(answers, function(a) as.numeric(json_field(a$grids, "bathymetry", NA_real_)), numeric(1)))
}

# The lookup for every record: one row per element of lon/lat, NA where the
# position is not a valid number. Different positions are looked up once.
xy_for_records <- function(dataset, lon, lat) {
  valid <- !is.na(lon) & !is.na(lat) & abs(lat) <= 90 & abs(lon) <= 180
  if (!any(valid)) return(data.table(shoredistance = rep(NA_real_, length(lon)), bathymetry = rep(NA_real_, length(lon))))
  position <- rep(NA_character_, length(lon))
  # Building text keys for 8 million records is slow: first find the different positions.
  codes <- value_codes(lon[valid]) * (length(lat) + 1) + value_codes(lat[valid])
  first <- which(!duplicated(codes))
  keys <- position_key(lon[valid][first], lat[valid][first])
  position[valid] <- keys[match(codes, codes[first])]
  looked_up <- xy_lookup(dataset, lon[valid][first], lat[valid][first])
  looked_up[match(position, looked_up$query)]
}

# On land, further than buffer_m from the coast? Returns, per record:
# on_land (logical, NA for invalid positions) and distance_km (inland), plus
# the attribute "source".
land_at <- function(dataset, lon, lat, buffer_m) {
  online_answer <- tryCatch(xy_for_records(dataset, lon, lat), qc_not_performed = function(condition) condition)
  if (!inherits(online_answer, "condition")) {
    result <- data.table(on_land = online_answer$shoredistance < -buffer_m, distance_km = round(-online_answer$shoredistance / 1000, 1))
    return(structure(result, source = "OBIS xylookup"))
  }
  if (!requireNamespace("maps", quietly = TRUE)) {
    not_performed(paste(conditionMessage(online_answer), "- and the package 'maps' for the offline coastline is not installed"))
  }
  structure(data.table(on_land = land_offline(lon, lat, buffer_m), distance_km = NA_real_),
            source = "offline coastline (approximate)")
}

# Offline: a position counts as on land beyond the buffer when it and 8 points
# around it, at the buffer distance, all lie on land in the 'maps' world map.
land_offline <- function(lon, lat, buffer_m) {
  valid <- !is.na(lon) & !is.na(lat) & abs(lat) <= 90 & abs(lon) <= 180
  on_land <- rep(NA, length(lon))
  if (!any(valid)) return(on_land)
  codes <- value_codes(lon[valid]) * (length(lat) + 1) + value_codes(lat[valid])
  first <- which(!duplicated(codes))
  x <- lon[valid][first]
  y <- lat[valid][first]
  inland <- !is.na(maps::map.where("world", x, y))
  for (angle in seq(0, 315, by = 45) * pi / 180) {
    still <- which(inland)
    if (!length(still)) break
    dy <- buffer_m / 111320 * cos(angle)
    dx <- buffer_m / (111320 * pmax(cos(y[still] * pi / 180), 0.01)) * sin(angle)
    inland[still] <- !is.na(maps::map.where("world", x[still] + dx, pmin(pmax(y[still] + dy, -90), 90)))
  }
  on_land[valid] <- inland[match(codes, codes[first])]
  on_land
}
