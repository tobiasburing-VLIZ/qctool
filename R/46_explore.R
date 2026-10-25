# =============================================================================
# A look at the data, for the app's Explore screen and the console
# (docs/design.md, section 6: "visual checks")
#
#   qc_time_summary()   records per year or month, the time span in the
#                       metadata, and where the middle 95 % of the dates lie
#   qc_taxa_summary()   records per taxon, with WoRMS details from the stored
#                       lookups (no new lookups) and the taxonomy issues
#   qc_emof_summary()   per measurement type and unit: records, numeric range,
#                       text examples, NERC label, issues
#   qc_event_levels()   the event hierarchy: events per level
#
# Like the summaries in 45_summaries.R these add no rules; they show the data.
# Built for big datasets: every value is looked at once (value_codes, per_value).
# =============================================================================

#' Records over time, compared with the time span in the metadata
#'
#' @param result A result of [qc_check()].
#' @return A list: `counts` (period, records), `unit` ("year" or "month"),
#'   `band` (dates between which the middle 95 % of the records lie),
#'   `coverage` (begin, end: the time span(s) in the metadata), `undated`
#'   (records without a valid date) and `outside_coverage` (records outside
#'   the metadata time span). NULL when there is no eventDate.
#' @export
qc_time_summary <- function(result) {
  dataset <- result$dataset
  table_name <- intersect(c("occurrence", "event"), names(dataset$tables))
  table_name <- table_name[vapply(table_name, function(t) field_available(dataset, t, "eventDate"), logical(1))][1]
  if (is.na(table_name)) return(NULL)

  # Each different date once, with the number of records that have it.
  values <- clean_dates(inherited_value(dataset, table_name, "eventDate")$value)
  codes <- value_codes(values)
  first <- which(!duplicated(codes))
  range <- iso_date_range(values[first])
  dates <- data.table(start = range$start, end = range$end, records = tabulate(codes)[codes[first]])
  undated <- sum(dates[is.na(start), records])
  dates <- dates[!is.na(start)]
  if (!nrow(dates)) return(list(counts = data.table(period = as.Date(character()), records = integer()), unit = "year",
                                band = as.Date(c(NA, NA)), coverage = eml_time_spans(dataset), undated = undated,
                                outside_coverage = 0L, table = table_name))

  span_years <- as.numeric(diff(range(dates$start))) / 365.25
  unit <- if (span_years > 3) "year" else "month"
  dates[, period := as.Date(format(start, if (unit == "year") "%Y-01-01" else "%Y-%m-01"))]
  counts <- dates[, .(records = sum(records)), by = period][order(period)]

  ordered <- dates[order(start)]
  share <- cumsum(ordered$records) / sum(ordered$records)
  band <- ordered$start[c(which(share >= 0.025)[1], which(share >= 0.975)[1])]

  coverage <- eml_time_spans(dataset)
  outside <- if (nrow(coverage)) {
    inside <- Reduce(`|`, lapply(seq_len(nrow(coverage)), function(i) dates$start >= coverage$begin[i] & dates$end <= coverage$end[i]))
    sum(dates$records[!inside])
  } else NA_integer_
  list(counts = counts, unit = unit, band = band, coverage = coverage, undated = undated, outside_coverage = outside, table = table_name)
}

# The time spans of the metadata (temporal coverage) as dates.
eml_time_spans <- function(dataset) {
  periods <- dataset$metadata$temporal
  if (is.null(periods) || !NROW(periods)) return(data.table(begin = as.Date(character()), end = as.Date(character())))
  begin <- iso_date_range(ifelse(is.na(periods$single), periods$begin, periods$single))$start
  end <- iso_date_range(ifelse(is.na(periods$single), periods$end, periods$single))$end
  data.table(begin = begin, end = end)[!is.na(begin) & !is.na(end)]
}

#' Records per taxon
#'
#' WoRMS details (rank, status, accepted name, habitat) come from the answers
#' stored by earlier lookups; no new lookups are made.
#'
#' @param result A result of [qc_check()].
#' @return A data.table, most records first: scientificName, scientificNameID,
#'   records, rank, status, accepted_name, habitat, issues (the taxonomy checks
#'   that found something for this taxon).
#' @export
qc_taxa_summary <- function(result) {
  dataset <- result$dataset
  occurrences <- dataset$tables$occurrence
  if (is.null(occurrences) || !nrow(occurrences)) return(data.table())
  names <- column_or_na(occurrences, "scientificName")
  ids <- column_or_na(occurrences, "scientificNameID")

  # Each different (name, ID) pair once.
  pair <- value_codes(names) * (length(names) + 1) + value_codes(ids)
  first <- which(!duplicated(pair))
  taxon_of_row <- match(pair, pair[first])
  taxa <- data.table(scientificName = names[first], scientificNameID = ids[first], records = tabulate(taxon_of_row, length(first)))

  # (An empty cache has none of the WoRMS columns: column_or_na() gives NA then.)
  stored <- cache_read("worms_records")
  worms <- stored[match(aphia_id(taxa$scientificNameID), stored$query)]
  from_worms <- function(field) column_or_na(worms, field)
  taxa[, `:=`(rank = from_worms("rank"), status = from_worms("status"),
              accepted_name = fifelse(!is.na(from_worms("valid_id")) & from_worms("valid_id") != worms$query,
                                      from_worms("valid_name"), NA_character_),
              habitat = habitat_text(worms))]

  taxonomy_checks <- c(result$checks[group == "taxonomy", check_id], "marine_taxon_on_land")
  problems <- result$issues_grouped[check_id %chin% taxonomy_checks & table == "occurrence" & records > 0]
  rows <- result$issue_rows[problem %in% problems$problem]
  if (nrow(rows)) {
    found <- data.table(taxon = taxon_of_row[rows$row], check = problems$check_id[match(rows$problem, problems$problem)])
    per_taxon <- unique(found)[, .(issues = paste(sort(check), collapse = ", ")), by = taxon]
    taxa[per_taxon$taxon, issues := per_taxon$issues]
  } else {
    taxa[, issues := NA_character_]
  }
  taxa[order(-records)]
}

habitat_text <- function(worms) {
  if (!nrow(worms) || is.null(worms$is_marine)) return(rep(NA_character_, nrow(worms)))
  parts <- cbind(ifelse(worms$is_marine %in% 1, "marine", NA), ifelse(worms$is_brackish %in% 1, "brackish", NA),
                 ifelse(worms$is_freshwater %in% 1, "freshwater", NA), ifelse(worms$is_terrestrial %in% 1, "terrestrial", NA))
  text <- apply(parts, 1, function(p) paste(stats::na.omit(p), collapse = ", "))
  ifelse(nzchar(text), text, NA_character_)
}

#' Measurements per type and unit
#'
#' @param result A result of [qc_check()].
#' @return A data.table, most records first: measurementType,
#'   measurementTypeID, nerc_label (from the stored NERC lookups),
#'   measurementUnit, measurementUnitID, records, numbers (how many values are
#'   numbers), min, median, max, text_examples, issues (issue rows from the
#'   checks on the eMoF) and checks. NULL when there is no eMoF.
#' @export
qc_emof_summary <- function(result) {
  dataset <- result$dataset
  name <- emof_table_name(dataset)
  if (is.na(name)) return(NULL)
  emof <- dataset$tables[[name]]
  columns <- c("measurementType", "measurementTypeID", "measurementUnit", "measurementUnitID")

  # One number per (type, typeID, unit, unitID) combination, built column by column.
  group <- rep(1L, nrow(emof))
  for (column in columns) group <- value_codes(group * (nrow(emof) + 1) + value_codes(column_or_na(emof, column)))
  first <- which(!duplicated(group))
  summary <- as.data.table(lapply(stats::setNames(columns, columns), function(column) column_or_na(emof, column)[first]))
  summary[, group := group[first]]

  number <- as_number(emof$measurementValue)
  stats <- data.table(group = group, number = number)[, .(records = .N, numbers = sum(!is.na(number)),
                                                          min = suppressWarnings(min(number, na.rm = TRUE)),
                                                          median = stats::median(number, na.rm = TRUE),
                                                          max = suppressWarnings(max(number, na.rm = TRUE))), by = group]
  stats[is.infinite(min), min := NA]
  stats[is.infinite(max), max := NA]
  summary <- merge(summary, stats, by = "group")

  # Up to three different text values per combination.
  text_rows <- which(is.na(number) & !is.na(emof$measurementValue))
  if (length(text_rows)) {
    texts <- unique(data.table(group = group[text_rows], value = emof$measurementValue[text_rows]))
    examples <- texts[, .(text_examples = paste(utils::head(value, 3), collapse = " | ")), by = group]
    summary <- merge(summary, examples, by = "group", all.x = TRUE)
  } else {
    summary[, text_examples := NA_character_]
  }

  stored <- cache_read("nerc_terms")
  summary[, nerc_label := column_or_na(stored, "label")[match(nerc_canonical(measurementTypeID), stored$query)]]

  problems <- result$issues_grouped[table == name & records > 0]
  rows <- result$issue_rows[problem %in% problems$problem]
  if (nrow(rows)) {
    found <- data.table(group = group[rows$row], check = problems$check_id[match(rows$problem, problems$problem)])
    per_group <- found[, .(issues = .N, checks = paste(sort(unique(check)), collapse = ", ")), by = group]
    summary <- merge(summary, per_group, by = "group", all.x = TRUE)
    summary[is.na(issues), issues := 0L]
  } else {
    summary[, `:=`(issues = 0L, checks = NA_character_)]
  }
  summary[order(-records), .(measurementType, measurementTypeID, nerc_label, measurementUnit, measurementUnitID, records,
                             numbers, min, median, max, text_examples, issues, checks)]
}

#' The event hierarchy: events per level
#'
#' Level 1 are the events without a parent (e.g. cruises), level 2 their
#' children (e.g. stations), and so on.
#'
#' @param result A result of [qc_check()].
#' @return A data.table: level, events, with_occurrences, occurrences,
#'   with_measurements, own_date, own_position, examples. NULL without an event table.
#' @export
qc_event_levels <- function(result) {
  dataset <- result$dataset
  events <- dataset$tables$event
  if (is.null(events) || !nrow(events)) return(NULL)
  n <- nrow(events)
  parent <- event_parent_row(dataset)
  broken <- !is.na(column_or_na(events, "parentEventID")) & is.na(parent)

  level <- rep(NA_integer_, n)
  level[is.na(parent) & !broken] <- 1L
  for (step in 2:50) {
    todo <- which(is.na(level) & !is.na(parent) & !is.na(level[parent]))
    if (!length(todo)) break
    level[todo] <- level[parent[todo]] + 1L
  }

  occurrences <- tabulate(occurrence_event_row(dataset), n)
  measurements <- if (is.na(emof_table_name(dataset))) integer(n) else tabulate(emof_event_row(dataset), n)
  ids <- column_or_na(events, "eventID")
  levels <- data.table(level = ifelse(is.na(level), "parent not found", as.character(level)), occurrences, measurements,
                       own_date = !is.na(column_or_na(events, "eventDate")),
                       own_position = !is.na(column_or_na(events, "decimalLatitude")), id = ids)
  levels[, .(events = .N, with_occurrences = sum(occurrences > 0), occurrences = sum(occurrences),
             with_measurements = sum(measurements > 0), own_date = sum(own_date), own_position = sum(own_position),
             examples = paste(utils::head(id[!is.na(id)], 3), collapse = "; ")), by = level][order(level)]
}
