# =============================================================================
# Checks, group "dates"
#
#   eventdate_iso8601               eventdate_eurobis_compatible
#   eventdate_not_future            eventdate_not_suspiciously_old
#   eventdate_interval_order        year_month_day_valid
#   year_month_day_match_eventdate  eventtime_valid
#   child_date_within_parent        dates_within_eml_coverage
#
# Dates are checked where they are written (a wrong date on a cruise event is
# reported once, on that event). Helpers: is_iso8601(), iso_date_range(),
# suggest_iso_date() in R/40_helpers.R.
# =============================================================================

# The eventDate columns of the dataset, per table, ignoring placeholders and
# leading/trailing spaces (reported by their own checks).
event_dates <- function(dataset) {
  tables <- tables_with_fields(dataset, "eventDate")
  if (!length(tables)) skip_check("no eventDate column")
  tables
}

clean_dates <- function(values) {
  values <- per_value(values, trimws)
  values[is_placeholder(values)] <- NA_character_
  values
}

# -----------------------------------------------------------------------------
# CHECK  eventdate_iso8601
# What:  eventDate is a valid ISO 8601 date, date-time or interval.
# Why:   dates in other formats (21/05/2019, May 2019) are ambiguous.
# -----------------------------------------------------------------------------
check_eventdate_iso8601 <- function(dataset, settings) {
  combine_issues(lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    rows <- rows_where(values, function(v) !is_iso8601(v))
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = "Not a valid ISO 8601 date (e.g. 2019-05-21, 2019-05, 2019-05-21/2019-05-25)",
               suggestion = suggest_iso_date(values[rows]))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  eventdate_eurobis_compatible
# What:  shortened intervals such as '2011-07/09' (valid ISO 8601, but the
#        EurOBIS harvester cannot read them). Suggests the full form.
#        An interval whose end is only a time ('2019-05-21T10:00/12:00') is a
#        warning, not an error (decision 2026-10-25).
# -----------------------------------------------------------------------------
check_eventdate_eurobis_compatible <- function(dataset, settings) {
  combine_issues(lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    rows <- rows_where(values, function(v) is_iso8601(v) & is_short_interval(v))
    if (!length(rows)) return(empty_issues())
    start <- sub("/.*$", "", values[rows])
    end <- sub("^.*/", "", values[rows])
    time_only <- grepl(":", end, fixed = TRUE) & is_iso8601_time(end)   # '12:00'; a bare '09' is a month or day
    full <- ifelse(time_only, paste0(start, "/", sub("[T ].*$", "", start), "T", end),
                   paste(start, full_interval_end(start, end), sep = "/"))
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = ifelse(time_only, "Interval whose end is only a time: valid ISO 8601, but the EurOBIS harvester may not read it",
                                "Shortened date interval: valid ISO 8601, but the EurOBIS harvester cannot read it"),
               suggestion = full,
               severity_hint = ifelse(time_only, "warning", NA_character_))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  eventdate_not_future
# What:  no eventDate (or interval end) lies after today.
# -----------------------------------------------------------------------------
check_eventdate_not_future <- function(dataset, settings) {
  combine_issues(lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    rows <- rows_where(values, function(v) iso_date_range(v)$end > Sys.Date())
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = "The date lies in the future")
  }))
}

# -----------------------------------------------------------------------------
# CHECK  eventdate_not_suspiciously_old
# What:  no eventDate before a threshold year, and no typical default dates.
# Why:   very old dates are often typos (1019 for 2019) or defaults (1900-01-01).
# Settings: min_year (1600), suspicious_dates (1900-01-01, 1970-01-01)
# -----------------------------------------------------------------------------
check_eventdate_not_suspiciously_old <- function(dataset, settings) {
  min_year <- setting(settings, "min_year", 1600)
  suspicious <- as.character(setting(settings, "suspicious_dates", c("1900-01-01", "1970-01-01")))
  combine_issues(lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    too_old <- rows_where(values, function(v) as.integer(format(iso_date_range(v)$start, "%Y")) < min_year)
    default_date <- rows_where(values, function(v) sub("[T ].*$", "", v) %in% suspicious)
    combine_issues(list(
      new_issues(dataset, table_name, too_old, field = "eventDate", value = values[too_old],
                 message = sprintf("The date lies before %d: a typo?", min_year)),
      new_issues(dataset, table_name, setdiff(default_date, too_old), field = "eventDate",
                 value = values[setdiff(default_date, too_old)],
                 message = "A typical default date (e.g. 1900-01-01): is this the real date?")))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  eventdate_interval_order
# What:  in an interval (start/end) the start is not after the end.
# -----------------------------------------------------------------------------
check_eventdate_interval_order <- function(dataset, settings) {
  combine_issues(lapply(event_dates(dataset), function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    rows <- rows_where(values, function(v) {
      range <- iso_date_range(v)
      grepl("/", v, fixed = TRUE) & !is.na(range$start) & range$start > range$end
    })
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = "The start of the interval is after its end")
  }))
}

# -----------------------------------------------------------------------------
# CHECK  year_month_day_valid
# What:  year is a 4-digit year not in the future, month 1-12, day 1-31 and
#        existing in that month.
# -----------------------------------------------------------------------------
check_year_month_day_valid <- function(dataset, settings) {
  tables <- unique(c(tables_with_fields(dataset, "year"), tables_with_fields(dataset, "month"), tables_with_fields(dataset, "day")))
  if (!length(tables)) skip_check("no year, month or day column")
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  combine_issues(lapply(tables, function(table_name) {
    data <- dataset$tables[[table_name]]
    number <- function(field) if (field %in% names(data)) as_integer(data[[field]]) else rep(NA_integer_, nrow(data))
    written <- function(field) if (field %in% names(data)) !is.na(data[[field]]) & !is_placeholder(data[[field]]) else rep(FALSE, nrow(data))
    year <- number("year"); month <- number("month"); day <- number("day")

    bad_year <- which(written("year") & (is.na(year) | year < 1000 | year > this_year))
    bad_month <- which(written("month") & (is.na(month) | month < 1 | month > 12))
    bad_day <- which(written("day") & (is.na(day) | day < 1 | day > 31))
    impossible <- which(!is.na(year) & !is.na(month) & !is.na(day) & day >= 1 & day <= 31 & month >= 1 & month <= 12 &
                        is.na(as.Date(sprintf("%04d-%02d-%02d", year, month, day), format = "%Y-%m-%d")))
    combine_issues(list(
      new_issues(dataset, table_name, bad_year, field = "year", value = data$year[bad_year],
                 message = "Not a 4-digit year up to this year"),
      new_issues(dataset, table_name, bad_month, field = "month", value = data$month[bad_month],
                 message = "Not a month number 1 to 12"),
      new_issues(dataset, table_name, bad_day, field = "day", value = data$day[bad_day],
                 message = "Not a day number 1 to 31",
                 suggestion = ifelse(suppressWarnings(as.integer(data$day[bad_day])) %in% 32:366, "a day of the year? use startDayOfYear", NA_character_)),
      new_issues(dataset, table_name, impossible, field = "year; month; day",
                 value = sprintf("%s-%s-%s", year[impossible], month[impossible], day[impossible]),
                 message = "This day does not exist in this month")))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  year_month_day_match_eventdate
# What:  if year, month or day are filled, they agree with eventDate (own or inherited).
# -----------------------------------------------------------------------------
check_year_month_day_match_eventdate <- function(dataset, settings) {
  tables <- intersect(c("event", "occurrence"), unique(c(tables_with_fields(dataset, "year"),
                                                         tables_with_fields(dataset, "month"), tables_with_fields(dataset, "day"))))
  if (!length(tables)) skip_check("no year, month or day column")
  combine_issues(lapply(tables, function(table_name) {
    data <- dataset$tables[[table_name]]
    range <- iso_date_range(clean_dates(inherited_value(dataset, table_name, "eventDate")$value))
    compare <- function(field, from, to, format_code) {
      if (!field %in% names(data)) return(NULL)
      value <- as_integer(data[[field]])
      start <- as.integer(format(range$start, format_code))
      end <- as.integer(format(range$end, format_code))
      same_period <- if (field == "year") TRUE else format(range$start, from) == format(range$end, to)
      rows <- which(!is.na(value) & !is.na(start) & same_period & (value < start | value > end))
      new_issues(dataset, table_name, rows, field = field, value = data[[field]][rows],
                 message = sprintf("%s does not agree with eventDate", field),
                 suggestion = ifelse(start[rows] == end[rows], as.character(start[rows]), paste(start[rows], "to", end[rows])))
    }
    combine_issues(list(compare("year", "%Y", "%Y", "%Y"), compare("month", "%Y", "%Y", "%m"), compare("day", "%Y-%m", "%Y-%m", "%d")))
  }))
}

# -----------------------------------------------------------------------------
# CHECK  eventtime_valid
# What:  eventTime is a valid ISO 8601 time (14:30, 14:30:00Z, 14:30+01:00) or
#        time interval (14:30/15:10).
# -----------------------------------------------------------------------------
check_eventtime_valid <- function(dataset, settings) {
  tables <- tables_with_fields(dataset, "eventTime")
  if (!length(tables)) skip_check("no eventTime column")
  combine_issues(lapply(tables, function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventTime)
    rows <- rows_where(values, function(v) {
      parts <- strsplit(v, "/", fixed = TRUE)
      !vapply(parts, function(p) length(p) %in% 1:2 && all(is_iso8601_time(p)), logical(1))
    })
    new_issues(dataset, table_name, rows, field = "eventTime", value = values[rows],
               message = "Not a valid ISO 8601 time (e.g. 14:30, 14:30:00Z, 14:30+01:00)",
               suggestion = suggest_iso_time(values[rows]))
  }))
}

# "14h30" -> "14:30", "2:30 PM" -> "14:30", "9:05" -> "09:05"
suggest_iso_time <- function(x) {
  m <- regmatches(x, regexec("(?i)^(\\d{1,2})[h:.](\\d{2})(?::(\\d{2}))?\\s*(am|pm)?$", x, perl = TRUE))
  vapply(m, function(parts) {
    if (length(parts) != 5) return(NA_character_)
    hour <- as.integer(parts[2])
    if (tolower(parts[5]) == "pm" && hour < 12) hour <- hour + 12L
    if (tolower(parts[5]) == "am" && hour == 12) hour <- 0L
    if (hour > 23) return(NA_character_)
    paste0(sprintf("%02d:%s", hour, parts[3]), if (nzchar(parts[4])) paste0(":", parts[4]) else "")
  }, character(1))
}

# -----------------------------------------------------------------------------
# CHECK  child_date_within_parent
# What:  the eventDate of an event falls within the eventDate of its parent event.
# Why:   a sample cannot be taken outside the time span of its cruise or station.
# -----------------------------------------------------------------------------
check_child_date_within_parent <- function(dataset, settings) {
  events <- require_fields(dataset, "event", c("eventDate", "parentEventID"))
  parent <- event_parent_row(dataset)
  own <- iso_date_range(clean_dates(events$eventDate))
  parent_range <- iso_date_range(clean_dates(inherited_value(dataset, "event", "eventDate")$value))[parent]
  rows <- which(!is.na(own$start) & !is.na(parent_range$start) &
                (own$start < parent_range$start | own$end > parent_range$end))
  new_issues(dataset, "event", rows, field = "eventDate", value = events$eventDate[rows],
             message = "The date lies outside the date of the parent event",
             suggestion = paste("parent event:", format(parent_range$start[rows]), "to", format(parent_range$end[rows])))
}

# -----------------------------------------------------------------------------
# CHECK  dates_within_eml_coverage
# What:  all eventDates fall within the temporal coverage of the metadata; also
#        reports a coverage much wider than the data.
# Settings: coverage_wider_days (365): how much wider the coverage may be
# -----------------------------------------------------------------------------
check_dates_within_eml_coverage <- function(dataset, settings) {
  wider_days <- setting(settings, "coverage_wider_days", 365)
  periods <- eml_periods(dataset)
  tables <- event_dates(dataset)
  data_start <- as.Date(NA)
  data_end <- as.Date(NA)

  issues <- lapply(tables, function(table_name) {
    values <- clean_dates(dataset$tables[[table_name]]$eventDate)
    range <- iso_date_range(values)
    data_start <<- suppressWarnings(min(c(data_start, range$start), na.rm = TRUE))
    data_end <<- suppressWarnings(max(c(data_end, range$end), na.rm = TRUE))
    # Inside when the date overlaps at least one period of the coverage.
    inside <- Reduce(`|`, lapply(seq_len(nrow(periods)), function(p) {
      !is.na(range$start) & range$end >= periods$start[p] & range$start <= periods$end[p]
    }))
    rows <- which(!is.na(range$start) & !inside)
    new_issues(dataset, table_name, rows, field = "eventDate", value = values[rows],
               message = "The date lies outside the temporal coverage of the metadata",
               suggestion = paste("metadata:", paste(format(periods$start), "to", format(periods$end), collapse = "; ")))
  })

  if (is.finite(data_start) && (min(periods$start) < data_start - wider_days || max(periods$end) > data_end + wider_days)) {
    issues[[length(issues) + 1]] <- new_issues(
      dataset, "eml", file = "eml.xml", field = "temporalCoverage",
      value = paste(format(min(periods$start)), "to", format(max(periods$end))),
      message = sprintf("The temporal coverage in the metadata is much wider (more than %s days) than the dates in the data", wider_days),
      suggestion = paste("data:", format(data_start), "to", format(data_end)))
  }
  combine_issues(issues)
}

# The periods of the EML temporal coverage as dates; skips the check when there are none.
eml_periods <- function(dataset) {
  temporal <- dataset$metadata$temporal
  if (is.null(temporal) || !nrow(temporal)) skip_check("no temporal coverage in the metadata")
  begin <- ifelse(is.na(temporal$begin), temporal$single, temporal$begin)
  end <- ifelse(is.na(temporal$end), ifelse(is.na(temporal$single), temporal$begin, temporal$single), temporal$end)
  periods <- data.table(start = iso_date_range(begin)$start, end = iso_date_range(end)$end)
  periods <- periods[!is.na(start) & !is.na(end)]
  if (!nrow(periods)) skip_check("the temporal coverage in the metadata has no readable dates")
  periods
}
