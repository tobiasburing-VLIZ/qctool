# =============================================================================
# Small helpers used by many checks
#
# Speed rule: datasets can have millions of rows, but far fewer DIFFERENT
# values. So a test runs on the unique values once, and the result is mapped
# back to the rows (rows_where). Never loop over rows.
# =============================================================================

# Apply f to each DIFFERENT value once and map the results back. For long
# columns (e.g. 34 million eMoF URIs with a handful of different values) this
# turns minutes into a second. f must work on a vector, one result per value.
per_value <- function(x, f) {
  if (length(x) < 100000) return(f(x))
  values <- unique(x)
  f(values)[if (is.character(x)) chmatch(x, values) else match(x, values)]
}

# Without surrounding spaces, and in lower case: " Present" -> "present".
# (trimws() on 34 million values takes minutes; on the different values, a second.)
lower_trim <- function(x) per_value(x, function(v) tolower(trimws(v)))

# Text to whole number ("12", " 3 "); NA for anything else, without warnings.
as_integer <- function(x) per_value(x, function(v) suppressWarnings(as.integer(trimws(v))))

# A number for each different value: c("b", "a", "b") -> 1 2 1. Grouping by
# numbers is several times faster than grouping by text.
value_codes <- function(x) {
  values <- unique(x)
  if (is.character(x)) chmatch(x, values) else match(x, values)
}

# Rows of x whose value passes test(). The test sees each different value once.
#   rows_where(c("a ", "b", "a "), function(v) grepl(" $", v))  ->  1 3
rows_where <- function(x, test) {
  values <- without_na(unique(x))
  if (!length(values)) return(integer())
  flagged <- values[which(test(values))]   # which(): a test result NA counts as "not flagged"
  if (!length(flagged)) return(integer())
  which(x %chin% flagged)
}

# Like rows_where, for checks that give a different message per value.
# describe() gets the different values and returns a message for each (NA = fine).
# Result: one row per flagged record with its message.
find_problem_values <- function(x, describe) {
  values <- without_na(unique(x))
  none <- data.table(row = integer(), message = character())
  if (!length(values)) return(none)
  messages <- describe(values)
  flagged <- which(!is.na(messages))
  if (!length(flagged)) return(none)
  rows <- which(x %chin% values[flagged])
  data.table(row = rows, message = messages[flagged][match(x[rows], values[flagged])])
}

# grepl that skips text with invalid UTF-8 bytes (reported by text_encoding_valid)
# instead of failing on it.
text_matches <- function(x, pattern) {
  valid <- validUTF8(x)
  result <- logical(length(x))
  result[valid] <- grepl(pattern, x[valid], perl = TRUE)
  result
}

# Run make_issues(table_name, rows) for each table named in data$table.
# Used for issues about columns from several tables at once.
for_each_table_of <- function(data, make_issues) {
  combine_issues(lapply(unique(data$table), function(table_name) {
    make_issues(table_name, which(data$table == table_name))
  }))
}

# Run check_column(table_name, column_name, values) for every Darwin Core column
# of every table and combine the issues. Internal columns (.line, .coreid) and
# columns that meta.xml does not map (column_N) are left out.
for_each_column <- function(dataset, check_column, skip_fields = character()) {
  issue_list <- list()
  for (table_name in names(dataset$tables)) {
    table <- dataset$tables[[table_name]]
    for (column in setdiff(mapped_columns(table), skip_fields)) {
      issue_list[[length(issue_list) + 1]] <- check_column(table_name, column, table[[column]])
    }
  }
  combine_issues(issue_list)
}

# Darwin Core columns that meta.xml maps to a term (not column_N, not internal).
mapped_columns <- function(table) {
  grep("^column_\\d+$", data_columns(table), value = TRUE, invert = TRUE)
}

# The values that are not NA.
without_na <- function(x) x[!is.na(x)]

# Shorten long values for messages and reports. Text that is not valid UTF-8
# has no length in characters (NA) and is left as it is.
shorten <- function(x, max_characters = 100) {
  long <- which(!is.na(x) & nchar(x, type = "chars", allowNA = TRUE) > max_characters)
  x[long] <- paste0(substr(x[long], 1, max_characters - 1), "…")
  x
}

# --- Numbers ---------------------------------------------------------------------

# A plain decimal number with a dot: "12", "-3.5", "0.25", "1e-3". Not: "1,5", "12 m", " 3".
is_decimal_number <- function(x) {
  grepl("^[+-]?(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?$", x, perl = TRUE)
}

# A whole number: "0", "25", "-3". Not "2.0" or "1,000".
is_whole_number <- function(x) {
  grepl("^[+-]?\\d+$", x, perl = TRUE)
}

# Text to number for values that are plain decimal numbers; NA for anything else
# (no warnings). Spaces around the number are ignored.
as_number <- function(x) per_value(x, text_to_number)

text_to_number <- function(x) {
  x <- trimws(x)
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is_decimal_number(x)
  out[ok] <- as.numeric(x[ok])
  out
}

# A decimal-degree value for coordinates written otherwise; NA when unclear.
#   "52,1" -> "52.1"   "51°13'30\"N" -> "51.225"   "51 13.5 S" -> "-51.225"   "3.2W" -> "-3.2"
suggest_decimal_degrees <- function(x, latitude = TRUE) {
  limit <- if (latitude) 90 else 180
  pattern <- "^\\s*(-?\\d+(?:[.,]\\d+)?)\\s*[°ºd]?\\s*(?:(\\d+(?:[.,]\\d+)?)\\s*['′m]?\\s*)?(?:(\\d+(?:[.,]\\d+)?)\\s*[\"″s]?\\s*)?([NSEWnsew])?\\s*$"
  parts <- regmatches(x, regexec(pattern, x, perl = TRUE))
  vapply(parts, function(p) {
    if (length(p) != 5) return(NA_character_)
    number <- function(s) if (nzchar(s)) as.numeric(sub(",", ".", s)) else 0
    degrees <- number(p[2])
    value <- abs(degrees) + number(p[3]) / 60 + number(p[4]) / 3600
    negative <- degrees < 0 || toupper(p[5]) %in% c("S", "W")
    value <- if (negative) -value else value
    if (abs(value) > limit) return(NA_character_)
    format(round(value, 6), scientific = FALSE, drop0trailing = TRUE)
  }, character(1))
}

# --- Dates and times (ISO 8601) -------------------------------------------------------

# Building blocks of the ISO 8601 patterns (extended format, as Darwin Core
# recommends; the compact basic format 20190521 is also accepted).
iso_year     <- "\\d{4}"
iso_month    <- "(0[1-9]|1[0-2])"
iso_day      <- "(0[1-9]|[12]\\d|3[01])"
iso_day_of_year <- "(00[1-9]|0[1-9]\\d|[12]\\d\\d|3[0-5]\\d|36[0-6])"
iso_date     <- sprintf("(%1$s(-%2$s(-%3$s)?)?|%1$s-%4$s|%1$s%2$s%3$s)", iso_year, iso_month, iso_day, iso_day_of_year)
iso_zone     <- "(Z|[+-]([01]\\d|2[0-3])(:?[0-5]\\d)?)"
iso_time     <- sprintf("([01]\\d|2[0-3])(:?[0-5]\\d(:?[0-5]\\d([.,]\\d+)?)?)?%s?", iso_zone)
iso_date_time <- sprintf("%s([T ]%s)?", iso_date, iso_time)   # "T", or a space as EMODnet accepts
# End of an interval may be shortened: 2007-11-13/15, 2011-07/09, 2007-11-13T10:00/12:00
iso_short_end <- sprintf("(%s|%s(-%s)?([T ]%s)?|%s)", iso_date_time, "\\d{2}", "\\d{2}", iso_time, iso_time)

iso8601_pattern <- sprintf("^%s(/%s)?$", iso_date_time, iso_short_end)
iso_time_pattern <- sprintf("^%s$", iso_time)

# TRUE for valid ISO 8601 dates, date-times and intervals:
#   "2019", "2019-05", "2019-05-21", "2019-141", "2019-05-21T14:30Z",
#   "2019-05-21/2019-05-25", "2007-11-13/15", "2011-07/09", "1900/1909",
#   and with a space instead of the "T": "2013-08-12 19:47:00" (EMODnet
#   accepts it, decision 2026-10-24; FIFD writes its dates this way)
# FALSE for "21/05/2019", "May 2019", "2019-5-21", "2019-02-30", "Jan2008".
is_iso8601 <- function(x) {
  valid <- grepl(iso8601_pattern, x, perl = TRUE)
  # The pattern allows day 31 in every month; check real calendar dates too.
  full_dates <- regmatches(x, gregexpr("\\d{4}-\\d{2}-\\d{2}", x))
  calendar_ok <- vapply(full_dates, function(dates) {
    all(!is.na(as.Date(dates, format = "%Y-%m-%d")))
  }, logical(1))
  valid & calendar_ok
}

# TRUE for valid ISO 8601 times: "14:30", "14:30:15", "14:30Z", "14:30+01:00".
is_iso8601_time <- function(x) grepl(iso_time_pattern, x, perl = TRUE)

# First and last day covered by valid ISO 8601 dates and intervals:
#   "2019"                  -> 2019-01-01 .. 2019-12-31
#   "2019-05"               -> 2019-05-01 .. 2019-05-31
#   "2019-05-21T14:30Z"     -> 2019-05-21 .. 2019-05-21
#   "2007-11-13/15"         -> 2007-11-13 .. 2007-11-15
# Invalid values give NA. Each different value is parsed once.
iso_date_range <- function(x) {
  values <- unique(x[!is.na(x)])
  valid <- trimws(values[is_iso8601(trimws(values))])
  has_end <- grepl("/", valid, fixed = TRUE)
  start_part <- sub("/.*$", "", valid)
  end_part <- ifelse(has_end, full_interval_end(start_part, sub("^.*/", "", valid)), start_part)
  position <- match(x, values[is_iso8601(trimws(values))])
  data.table(start = date_part_as_date(start_part, last_day = FALSE)[position],
             end = date_part_as_date(end_part, last_day = TRUE)[position])
}

# Dates (time part ignored) as Date: the first or the last day each covers.
# Works on whole vectors: YYYY, YYYY-MM, YYYY-MM-DD, YYYYMMDD, YYYY-DDD.
date_part_as_date <- function(part, last_day) {
  date <- sub("[T ].*$", "", part)
  compact <- grepl("^\\d{8}$", date)
  date[compact] <- sprintf("%s-%s-%s", substr(date[compact], 1, 4), substr(date[compact], 5, 6), substr(date[compact], 7, 8))
  year <- suppressWarnings(as.integer(substr(date, 1, 4)))
  result <- rep(as.Date(NA), length(date))

  full <- grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
  result[full] <- as.Date(date[full], format = "%Y-%m-%d")

  ordinal <- grepl("^\\d{4}-\\d{3}$", date)
  result[ordinal] <- as.Date(sprintf("%04d-01-01", year[ordinal])) + as.integer(substr(date[ordinal], 6, 8)) - 1

  month_only <- grepl("^\\d{4}-\\d{2}$", date)
  month <- suppressWarnings(as.integer(substr(date, 6, 7)))
  first_of_month <- as.Date(sprintf("%04d-%02d-01", year, month), format = "%Y-%m-%d")
  next_month <- as.Date(sprintf("%04d-%02d-01", ifelse(month == 12, year + 1L, year), ifelse(month == 12, 1L, month + 1L)), format = "%Y-%m-%d")
  result[month_only] <- if (last_day) next_month[month_only] - 1 else first_of_month[month_only]

  year_only <- grepl("^\\d{4}$", date)
  result[year_only] <- as.Date(sprintf(if (last_day) "%04d-12-31" else "%04d-01-01", year[year_only]))
  result
}

# The end of an interval written in full: ("2007-11-13", "15") -> "2007-11-15",
# ("2011-07", "09") -> "2011-09". A time-only end keeps the start date. Vectorised.
full_interval_end <- function(start, end) {
  start_date <- sub("[T ].*$", "", start)
  end_date <- sub("[T ].*$", "", end)
  out <- end
  time_only <- grepl(":", end_date)
  out[time_only] <- start_date[time_only]                       # "12:00" after "...T10:00"
  short <- !grepl("^\\d{4}", end_date) & !grepl("^\\d{8}$", start_date) & !time_only
  if (any(short)) {
    out[short] <- mapply(function(s, e) {
      start_parts <- strsplit(s, "-", fixed = TRUE)[[1]]
      end_parts <- strsplit(e, "-", fixed = TRUE)[[1]]
      paste(c(start_parts[seq_len(length(start_parts) - length(end_parts))], end_parts), collapse = "-")
    }, start_date[short], end_date[short], USE.NAMES = FALSE)
  }
  out
}

# TRUE for intervals whose end is shortened ("2011-07/09"): valid ISO 8601,
# but the EurOBIS harvester cannot read them.
is_short_interval <- function(x) {
  grepl("/", x, fixed = TRUE) & !grepl("/\\d{4}", x)
}

# A proposed ISO 8601 form for dates written otherwise; NA when unclear.
suggest_iso_date <- function(x) {
  v <- trimws(x)
  out <- rep(NA_character_, length(v))
  set_if <- function(condition, value) {
    todo <- which(condition & is.na(out))
    out[todo] <<- value[todo]
  }

  months <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
  # 2019-5-21, 2019/05/21, 2019.05.21
  ymd <- regmatches(v, regexec("^(\\d{4})[-/.](\\d{1,2})[-/.](\\d{1,2})$", v))
  set_if(lengths(ymd) == 4, vapply(ymd, function(m) if (length(m) == 4) sprintf("%s-%02d-%02d", m[2], as.integer(m[3]), as.integer(m[4])) else NA_character_, ""))
  # 21/05/2019, 21.05.2019, 21-05-2019 (day first unless the first number cannot be a day)
  dmy <- regmatches(v, regexec("^(\\d{1,2})[-/.](\\d{1,2})[-/.](\\d{4})$", v))
  set_if(lengths(dmy) == 4, vapply(dmy, function(m) {
    if (length(m) != 4) return(NA_character_)
    a <- as.integer(m[2]); b <- as.integer(m[3])
    if (a > 12 && b <= 12) return(sprintf("%s-%02d-%02d", m[4], b, a))
    if (b > 12 && a <= 12) return(sprintf("%s-%02d-%02d", m[4], a, b))
    sprintf("%s-%02d-%02d or %s-%02d-%02d (unclear which is the day)", m[4], b, a, m[4], a, b)
  }, ""))
  # Jan2008, January 2008, jan-2008
  my <- regmatches(v, regexec("(?i)^([a-z]{3})[a-z]*[ ._-]?(\\d{4})$", v, perl = TRUE))
  set_if(lengths(my) == 3, vapply(my, function(m) {
    if (length(m) != 3 || is.na(months[tolower(m[2])])) return(NA_character_)
    sprintf("%s-%02d", m[3], months[[tolower(m[2])]])
  }, ""))
  # 21 May 2019
  dmy_text <- regmatches(v, regexec("(?i)^(\\d{1,2})[ .-]([a-z]{3})[a-z]*[ .-](\\d{4})$", v, perl = TRUE))
  set_if(lengths(dmy_text) == 4, vapply(dmy_text, function(m) {
    if (length(m) != 4 || is.na(months[tolower(m[3])])) return(NA_character_)
    sprintf("%s-%02d-%02d", m[4], months[[tolower(m[3])]], as.integer(m[2]))
  }, ""))
  # 2019-05-21 14:30 -> 2019-05-21T14:30
  set_if(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}", v), sub(" ", "T", v))
  # 1991-1994 -> 1991/1994
  years_range <- grepl("^\\d{4}-\\d{4}$", v)
  set_if(years_range & suppressWarnings(as.integer(substr(v, 6, 9)) > as.integer(substr(v, 1, 4))), sub("-", "/", v))
  # Excel date numbers (days since 1899-12-30)
  excel <- grepl("^\\d{5}$", v) & suppressWarnings(as.integer(v)) > 2000 & suppressWarnings(as.integer(v)) < 60000
  set_if(excel, paste(format(as.Date(suppressWarnings(as.integer(v)), origin = "1899-12-30")), "(this looks like an Excel date number)"))
  out
}

# --- Text --------------------------------------------------------------------------

# An identifier or link with a scheme: "http://...", "https://...", "urn:lsid:...", "doi:10...".
is_uri <- function(x) grepl("^[A-Za-z][A-Za-z0-9+.-]*:\\S+$", x, perl = TRUE)

# --- Text that is not valid UTF-8 -----------------------------------------------------
#
# Excel saves 'CSV' on Windows as Windows-1252, not UTF-8. Its accents ('é',
# '±', 'ø') are then bytes that are not valid UTF-8, and R's text functions
# (grepl, tolower, ...) stop on them: checks would fail one after the other.
# So the readers repair such values right after reading (repair_invalid_text)
# and remember where they were; the check text_encoding_valid reports them.

# Text as valid UTF-8: values that are not valid UTF-8 are read as Windows-1252
# (else Latin-1, which accepts every byte). Valid text is not touched. Works on
# a vector, a list or a table (a table is copied, never changed in place).
utf8_safe <- function(x) {
  if (is.data.frame(x)) {
    bad_columns <- names(x)[vapply(x, function(column) is.character(column) && !all(validUTF8(column)), logical(1))]
    bad_names <- !all(validUTF8(names(x)))
    if (!length(bad_columns) && !bad_names) return(x)
    x <- copy(as.data.table(x))   # the data itself stays as it was read
    for (column in bad_columns) set(x, j = column, value = utf8_safe(x[[column]]))
    if (bad_names) setnames(x, utf8_safe(names(x)))
    return(x)
  }
  if (is.list(x)) return(lapply(x, utf8_safe))
  if (!is.character(x)) return(x)
  bad <- which(!validUTF8(x))
  if (!length(bad)) return(x)
  fixed <- iconv(x[bad], from = "CP1252", to = "UTF-8")
  fixed[is.na(fixed)] <- iconv(x[bad][is.na(fixed)], from = "latin1", to = "UTF-8")
  x[bad] <- fixed
  x
}

# Repair a table just read from a file: column names and values that are not
# valid UTF-8 are read as Windows-1252 (see utf8_safe). Returns a list:
#   data     the table (the same object, repaired)
#   invalid  one row per repaired value: column (its name after the repair),
#            position (column number) and row; empty when all text was valid
# validUTF8() is quick: about 1 second for a column of 34 million values.
repair_invalid_text <- function(data) {
  if (!all(validUTF8(names(data)))) setnames(data, utf8_safe(names(data)))
  invalid <- list()
  for (position in seq_along(data)) {
    values <- data[[position]]
    if (!is.character(values)) next
    rows <- which(!validUTF8(values))
    if (!length(rows)) next
    invalid[[length(invalid) + 1]] <- data.table(column = names(data)[position], position = position, row = rows)
    set(data, i = rows, j = position, value = utf8_safe(values[rows]))
  }
  invalid <- if (length(invalid)) rbindlist(invalid) else data.table(column = character(), position = integer(), row = integer())
  list(data = data, invalid = invalid)
}

# The reading note for repaired text, e.g. "3 values in occurrence.txt were ...".
invalid_text_note <- function(invalid, file) {
  sprintf("%s values in %s were not valid UTF-8 (the file was probably saved as Windows-1252, e.g. by Excel) and were read as Windows-1252; text_encoding_valid lists them",
          format(nrow(invalid), big.mark = ","), file)
}
