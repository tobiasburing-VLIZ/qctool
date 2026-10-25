# =============================================================================
# More looks at the data (Explore screen and map), for a person to judge
#
#   qc_numeric_variables()  the numbers a dataset has: positions, depths, year,
#                           counts, and every eMoF measurement type + unit
#   qc_variable_values()    the values of one of them, per record, with the taxon
#   qc_relationship()       two measurements of the same records (e.g. length and
#                           weight), a crude fit and the records far from it
#   qc_sex_ratio()          males and females per taxon
#   qc_tracks()             positions in date order per cruise (or per
#                           individual, vessel, ...), for drawing tracks on a map
#
# These are no checks and add no issues: the fit is a simple model to find odd
# values by eye, not a scientific length-weight relationship.
# =============================================================================

#' The numbers a dataset has, to look at their distribution
#'
#' @param result A result of [qc_check()].
#' @param min_values Fewest numbers a measurement type needs to be listed.
#' @return A data.table: id, label, kind ("field", "year" or "emof"), table,
#'   field, type, unit, values.
#' @export
qc_numeric_variables <- function(result, min_values = 10) {
  dataset <- result$dataset
  base <- record_table(dataset)
  rows <- list()
  if (!is.na(base)) {
    n <- nrow(dataset$tables[[base]])
    for (field in c("decimalLatitude", "decimalLongitude", "minimumDepthInMeters", "maximumDepthInMeters",
                    "individualCount", "organismQuantity")) {
      if (field_available(dataset, base, field)) {
        rows[[field]] <- data.table(id = paste0("field:", field), label = field, kind = "field", table = base,
                                    field = field, type = NA_character_, unit = NA_character_, values = n)
      }
    }
    if (field_available(dataset, base, "eventDate")) {
      rows$year <- data.table(id = "year", label = "year (from eventDate)", kind = "year", table = base,
                              field = "eventDate", type = NA_character_, unit = NA_character_, values = n)
    }
  }
  name <- emof_table_name(dataset)
  if (!is.na(name) && "measurementValue" %in% names(dataset$tables[[name]])) {
    emof <- dataset$tables[[name]]
    numeric_rows <- which(!is.na(as_number(emof$measurementValue)))
    if (length(numeric_rows)) {
      type <- column_or_na(emof, "measurementType")[numeric_rows]
      unit <- column_or_na(emof, "measurementUnit")[numeric_rows]
      code <- value_codes(type) * (length(type) + 1) + value_codes(unit)
      first <- which(!duplicated(code))
      counts <- tabulate(match(code, code[first]), length(first))
      found <- data.table(type = type[first], unit = unit[first], values = counts)[values >= min_values][order(-values)]
      if (nrow(found)) {
        rows$emof <- found[, .(id = paste0("emof:", seq_len(.N)), label = ifelse(is.na(unit) | is_empty_value(unit), type, sprintf("%s (%s)", type, unit)),
                               kind = "emof", table = name, field = "measurementValue", type, unit, values)]
      }
    }
  }
  rbindlist(rows, fill = TRUE)
}

# The table whose rows are "the records": occurrences, or events when there are none.
record_table <- function(dataset) {
  intersect(c("occurrence", "event"), names(dataset$tables))[1]
}

#' The values of one numeric variable, per record, with the taxon
#'
#' @param result A result of [qc_check()].
#' @param variable An id from [qc_numeric_variables()], e.g. "field:decimalLatitude".
#' @param variables The result of [qc_numeric_variables()] (made when not given).
#' @return A data.table: owner (the row of the occurrence, or of the event when
#'   there are no occurrences; NA for eMoF rows that belong to an event only),
#'   value, taxon.
#' @export
qc_variable_values <- function(result, variable, variables = qc_numeric_variables(result)) {
  dataset <- result$dataset
  chosen <- variables[id == variable]
  if (!nrow(chosen)) stop("Unknown variable: ", variable, call. = FALSE)
  base <- record_table(dataset)
  taxa <- if (identical(base, "occurrence")) column_or_na(dataset$tables$occurrence, "scientificName") else NULL

  values <- if (chosen$kind == "field") {
    data.table(owner = seq_len(nrow(dataset$tables[[base]])), value = as_number(inherited_value(dataset, base, chosen$field)$value))
  } else if (chosen$kind == "year") {
    dates <- clean_dates(inherited_value(dataset, base, "eventDate")$value)
    data.table(owner = seq_along(dates), value = as.numeric(format(iso_date_range(dates)$start, "%Y")))
  } else {
    emof <- dataset$tables[[chosen$table]]
    same <- function(x, wanted) if (is.na(wanted)) is.na(x) else x %chin% wanted
    rows <- which(same(column_or_na(emof, "measurementType"), chosen$type) & same(column_or_na(emof, "measurementUnit"), chosen$unit))
    owner <- if (identical(base, "occurrence")) emof_owner_occurrence(dataset)[rows] else emof_event_row(dataset)[rows]
    data.table(owner = owner, value = as_number(emof$measurementValue[rows]))
  }
  values <- values[!is.na(value)]
  values[, taxon := if (is.null(taxa)) NA_character_ else taxa[owner]]
  values[]
}

#' Box plot numbers of a variable: for all records and per taxon
#'
#' @param values A table with the columns value and taxon, e.g. from [qc_variable_values()].
#' @param max_taxa At most this many taxa (those with most values) get their own box.
#' @return One row per group ("all records", then the taxa): n, min, lower
#'   (end of the lower whisker), q1, median, q3, upper (end of the upper
#'   whisker), max, beyond (values further than 1.5 box lengths from the box).
#' @export
qc_box_stats <- function(values, max_taxa = 15) {
  box <- function(v) {
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    reach <- 1.5 * (q[3] - q[1])
    inside <- v >= q[1] - reach & v <= q[3] + reach
    list(n = length(v), min = min(v), lower = min(v[inside]), q1 = q[1], median = q[2], q3 = q[3],
         upper = max(v[inside]), max = max(v), beyond = sum(!inside))
  }
  all_records <- data.table(group = "all records", as.data.table(box(values$value)))
  taxa <- values[!is.na(taxon), .N, by = taxon][order(-N)]
  if (nrow(taxa) < 2) return(all_records)
  top <- utils::head(taxa$taxon, max_taxa)
  per_taxon <- values[taxon %chin% top, box(value), by = taxon]
  setnames(per_taxon, "taxon", "group")
  rbind(all_records, per_taxon[match(top, group)])
}

# The occurrence each eMoF row belongs to: its own link, or else the only
# occurrence of its event (tracking and specimen data often put the
# measurements on an event with one occurrence). NA when it cannot be told.
emof_owner_occurrence <- function(dataset) {
  owner <- emof_occurrence_row(dataset)
  unlinked <- which(is.na(owner))
  if (length(unlinked) && !is.null(dataset$tables$event)) {
    occurrence_event <- occurrence_event_row(dataset)
    per_event <- tabulate(occurrence_event, nrow(dataset$tables$event))
    only_occurrence <- rep(NA_integer_, nrow(dataset$tables$event))
    single <- which(!is.na(occurrence_event) & per_event[occurrence_event] == 1)
    only_occurrence[occurrence_event[single]] <- single
    owner[unlinked] <- only_occurrence[emof_event_row(dataset)[unlinked]]
  }
  owner
}

#' Two measurements of the same records, a crude fit, and the records far from it
#'
#' For example length (x) against weight (y): the "power" model fits
#' log(weight) = a + b log(length), the usual length-weight relation. Records
#' whose residual is more than `threshold` times the typical residual (MAD)
#' away from the curve are marked. A tool to find odd values by eye, not a
#' scientific model.
#'
#' @param result A result of [qc_check()].
#' @param x,y Variable ids from [qc_numeric_variables()].
#' @param model "linear" (y = a + b x), "exponential" (y = a e^(b x)) or "power" (y = a x^b).
#' @param threshold How many typical residuals away counts as far.
#' @param taxon Optional: only this scientificName.
#' @param variables The result of [qc_numeric_variables()] (made when not given).
#' @return A list: `points` (owner, x, y, taxon, fitted, residual, far),
#'   `curve` (x, y of the fitted line), `formula` (text), `r_squared`; NULL
#'   when fewer than 5 records have both values.
#' @export
qc_relationship <- function(result, x, y, model = c("linear", "exponential", "power"), threshold = 3,
                            taxon = NULL, variables = qc_numeric_variables(result)) {
  model <- match.arg(model)
  first_value <- function(values) values[!is.na(owner) & !duplicated(owner)]
  xs <- first_value(qc_variable_values(result, x, variables))
  ys <- first_value(qc_variable_values(result, y, variables))
  points <- merge(xs[, .(owner, x = value, taxon)], ys[, .(owner, y = value)], by = "owner")
  wanted_taxon <- taxon                      # inside points[...] 'taxon' would mean the column
  if (length(wanted_taxon) && nzchar(wanted_taxon)) points <- points[taxon %chin% wanted_taxon]
  if (model == "exponential") points <- points[y > 0]
  if (model == "power") points <- points[x > 0 & y > 0]
  if (nrow(points) < 5) return(NULL)

  # Fit on a scale where the model is a straight line. Each different (x, y)
  # pair counts once: the same animal measured on every tracking position, or
  # many fish of the same size, should not decide what is "typical".
  to_line <- switch(model,
    linear      = list(x = function(v) v,      y = function(v) v,      back = function(v) v),
    exponential = list(x = function(v) v,      y = function(v) log(v), back = function(v) exp(v)),
    power       = list(x = function(v) log(v), y = function(v) log(v), back = function(v) exp(v)))
  distinct <- unique(points[, .(x, y)])
  if (nrow(distinct) < 3) return(NULL)
  fit_x <- to_line$x(distinct$x)
  fit_y <- to_line$y(distinct$y)
  # Fit twice: the odd values would pull a single fit towards them. The second
  # fit leaves out what lies far from the first one.
  fit <- stats::lm(fit_y ~ fit_x)
  first_residual <- stats::residuals(fit)
  first_typical <- stats::mad(first_residual)
  if (is.finite(first_typical) && first_typical > 0) {
    close <- abs(first_residual) <= threshold * first_typical
    if (sum(close) >= 3) fit <- stats::lm(fit_y ~ fit_x, subset = close)
  }
  a <- unname(stats::coef(fit)[1])
  b <- unname(stats::coef(fit)[2])
  typical <- stats::mad(fit_y - (a + b * fit_x))
  if (!is.finite(typical) || typical == 0) typical <- stats::sd(fit_y - (a + b * fit_x))
  line_x <- to_line$x(points$x)
  line_y <- to_line$y(points$y)
  residual <- line_y - (a + b * line_x)
  points[, `:=`(fitted = to_line$back(a + b * line_x), residual = residual,
                far = if (is.finite(typical) && typical > 0) abs(residual) > threshold * typical else FALSE)]

  grid <- seq(min(points$x), max(points$x), length.out = 200)
  curve <- data.table(x = grid, y = to_line$back(a + b * to_line$x(grid)))
  formula <- switch(model,
    linear      = sprintf("y = %.4g + %.4g x", a, b),
    exponential = sprintf("y = %.4g e^(%.4g x)", exp(a), b),
    power       = sprintf("y = %.4g x^%.4g", exp(a), b))
  list(points = points[], curve = curve, formula = formula, r_squared = summary(fit)$r.squared)
}

#' Males and females per taxon
#'
#' The sex comes from the occurrence field `sex`, or else from an eMoF
#' measurement whose type is sex or gender. "M", "male", "males" count as
#' male; "F", "female", "females" as female; everything else as other.
#'
#' @param result A result of [qc_check()].
#' @return A data.table, most records first: taxon, female, male, other,
#'   share_female (of the records that are male or female); NULL when there
#'   is no sex information.
#' @export
qc_sex_ratio <- function(result) {
  dataset <- result$dataset
  occurrences <- dataset$tables$occurrence
  if (is.null(occurrences)) return(NULL)
  sexes <- NULL
  if ("sex" %in% names(occurrences)) {
    sexes <- data.table(owner = seq_len(nrow(occurrences)), sex = occurrences$sex)[!is.na(sex)]
  } else if (!is.na(emof_table_name(dataset))) {
    emof <- dataset$tables[[emof_table_name(dataset)]]
    is_sex <- per_value(column_or_na(emof, "measurementType"), function(t) grepl("(?i)^\\s*(sex|gender)\\b", t, perl = TRUE)) |
      nerc_code(column_or_na(emof, "measurementTypeID")) %chin% "ENTSEX01"
    rows <- which(is_sex)
    if (length(rows)) sexes <- data.table(owner = emof_owner_occurrence(dataset)[rows], sex = emof$measurementValue[rows])[!is.na(owner)]
  }
  if (is.null(sexes) || !nrow(sexes)) return(NULL)
  kind <- per_value(sexes$sex, function(v) {
    v <- tolower(trimws(v))
    ifelse(v %in% c("m", "male", "males"), "male", ifelse(v %in% c("f", "female", "females"), "female", "other"))
  })
  counts <- data.table(taxon = column_or_na(occurrences, "scientificName")[sexes$owner], kind = kind)[
    , .(female = sum(kind == "female"), male = sum(kind == "male"), other = sum(kind == "other")), by = taxon]
  counts[, share_female := fifelse(female + male > 0, round(female / (female + male), 3), NA_real_)]
  counts[order(-(female + male + other))]
}

#' Positions in date order, as tracks
#'
#' @param result A result of [qc_check()].
#' @param by How to split into tracks: "top-level event" (the cruise: the event
#'   at the top of the hierarchy), a column name (e.g. "organismID",
#'   "parentEventID", "recordedBy"), or "one track".
#' @param max_tracks Most tracks (the longest ones).
#' @param max_points Most points per track (evenly thinned above that).
#' @return A data.table: track, lat, lon, date, in date order per track; NULL
#'   without positions and dates.
#' @export
qc_tracks <- function(result, by = "top-level event", max_tracks = 300, max_points = 3000) {
  dataset <- result$dataset
  base <- track_table(dataset, by)
  if (is.na(base) || !field_available(dataset, base, "decimalLatitude") || !field_available(dataset, base, "eventDate")) return(NULL)
  n <- nrow(dataset$tables[[base]])
  track <- if (by == "one track") rep("all", n) else if (by == "top-level event" && base == "event") {
    top_event_ids(dataset)
  } else if (by == "eventID, first part") {
    eventid_first_part(inherited_value(dataset, base, "eventID")$value)
  } else {
    inherited_value(dataset, base, by)$value
  }
  points <- data.table(track = track,
                       lat = as_number(inherited_value(dataset, base, "decimalLatitude")$value),
                       lon = as_number(inherited_value(dataset, base, "decimalLongitude")$value),
                       date = iso_date_range(clean_dates(inherited_value(dataset, base, "eventDate")$value))$start)
  points <- unique(points[!is.na(track) & !is.na(lat) & !is.na(lon) & !is.na(date) & abs(lat) <= 90 & abs(lon) <= 180])
  if (!nrow(points)) return(NULL)
  longest <- points[, .N, by = track][order(-N)][seq_len(min(.N, max_tracks)), track]
  points <- points[track %chin% longest][order(track, date)]
  points[, keep := seq_len(.N) %in% unique(round(seq(1, .N, length.out = min(.N, max_points)))), by = track]
  points[keep == TRUE, .(track, lat, lon, date)]
}

# For each event: the eventID of the event at the top of its hierarchy.
top_event_ids <- function(dataset) {
  events <- dataset$tables$event
  parent <- event_parent_row(dataset)
  top <- seq_len(nrow(events))
  for (step in 1:50) {
    up <- which(!is.na(parent[top]))
    if (!length(up)) break
    top[up] <- parent[top[up]]
  }
  column_or_na(events, "eventID")[top]
}

# The table whose positions make the tracks: the events, unless the grouping
# column is an occurrence column (e.g. organismID: one track per animal).
track_table <- function(dataset, by) {
  has_events <- "event" %in% names(dataset$tables) && field_available(dataset, "event", "decimalLatitude")
  field <- if (by == "eventID, first part") "eventID" else by
  if (has_events && (by %in% c("top-level event", "one track") || field_available(dataset, "event", field))) return("event")
  record_table(dataset)
}

# "163345_POP-UP_163345_2017-05-24" -> "163345": many datasets start the
# eventID with the cruise, station or tag.
eventid_first_part <- function(ids) per_value(ids, function(v) sub("[_-].*$", "", v))

# The ways tracks can be made for a dataset: the cruise (top-level event) when
# events have parents, and columns with between 2 and 2,000 different values.
track_groupings <- function(result) {
  dataset <- result$dataset
  if (is.na(record_table(dataset))) return(character())
  choices <- character()
  if ("event" %in% names(dataset$tables) && any(!is.na(event_parent_row(dataset)))) choices <- "top-level event"
  for (field in c("parentEventID", "organismID", "eventID, first part", "recordedBy", "datasetName", "locality",
                  "fieldNumber", "samplingProtocol")) {
    column <- if (field == "eventID, first part") "eventID" else field
    base <- track_table(dataset, field)
    if (!is.na(base) && field_available(dataset, base, column)) {
      values <- inherited_value(dataset, base, column)$value
      if (field == "eventID, first part") values <- eventid_first_part(values)
      different <- uniqueN(values, na.rm = TRUE)
      if (different >= 2 && different <= 2000) choices <- c(choices, field)
    }
  }
  c(choices, "one track")
}
