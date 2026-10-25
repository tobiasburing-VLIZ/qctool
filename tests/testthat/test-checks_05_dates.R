dates_archive <- function(dates, ...) {
  qc_read_dwca(make_archive(event = data.frame(id = paste0("E", seq_along(dates)), eventID = paste0("E", seq_along(dates)),
                                               eventDate = dates, ...)))
}

test_that("iso_date_range gives first and last day", {
  range <- iso_date_range(c("2019", "2019-02", "2019-05-21T14:30Z", "2007-11-13/15", "2011-07/09", "bad"))
  expect_equal(format(range$start), c("2019-01-01", "2019-02-01", "2019-05-21", "2007-11-13", "2011-07-01", NA))
  expect_equal(format(range$end), c("2019-12-31", "2019-02-28", "2019-05-21", "2007-11-15", "2011-09-30", NA))
})

test_that("suggest_iso_date proposes ISO forms", {
  expect_equal(suggest_iso_date(c("21/05/2019", "2019/5/21", "Jan2008", "2019-05-21 14:30", "1991-1994", "43606")),
               c("2019-05-21", "2019-05-21", "2008-01", "2019-05-21T14:30", "1991/1994", "2019-05-21 (this looks like an Excel date number)"))
  expect_match(suggest_iso_date("04/06/2013"), "unclear")
})

test_that("eventdate_iso8601 finds non-ISO dates with a suggestion", {
  issues <- run_check("eventdate_iso8601", dates_archive(c("2019-05-21", "21/05/2019", "NA")))
  expect_equal(issues$value, "21/05/2019")
  expect_equal(issues$suggestion, "2019-05-21")
})

test_that("eventdate_eurobis_compatible writes shortened intervals in full", {
  issues <- run_check("eventdate_eurobis_compatible", dates_archive(c("2011-07/09", "2011-07/2011-09", "2007-11-13/15")))
  expect_equal(issues$suggestion, c("2011-07/2011-09", "2007-11-13/2007-11-15"))
})

test_that("future, suspiciously old, default dates and reversed intervals", {
  future <- format(Sys.Date() + 400, "%Y-%m-%d")
  dataset <- dates_archive(c(future, "1019-05-21", "1900-01-01", "2020-05-01/2019-05-01", "2019-05-21"))
  expect_equal(run_check("eventdate_not_future", dataset)$value, future)
  old <- run_check("eventdate_not_suspiciously_old", dataset, list(min_year = 1600, suspicious_dates = c("1900-01-01")))
  expect_equal(old$value, c("1019-05-21", "1900-01-01"))
  expect_equal(run_check("eventdate_interval_order", dataset)$value, "2020-05-01/2019-05-01")
})

test_that("year, month, day: valid and matching eventDate", {
  dataset <- dates_archive(c("2019-05-21", "2019-05-21", "2019-02-10"),
                           year = c("2019", "2018", "2019"), month = c("5", "5", "2"), day = c("21", "218", "30"))
  valid <- run_check("year_month_day_valid", dataset)
  expect_equal(valid$field, c("day", "year; month; day"))
  expect_match(valid$suggestion[1], "startDayOfYear")
  match <- run_check("year_month_day_match_eventdate", dataset)
  expect_equal(match[field == "year", record_id], "E2")
})

test_that("eventtime_valid with suggestions", {
  issues <- run_check("eventtime_valid", dates_archive(c("2019", "2019", "2019"), eventTime = c("14:30Z", "14h30", "2:30 PM")))
  expect_equal(issues$suggestion, c("14:30", "14:30"))
})

test_that("child_date_within_parent", {
  dataset <- qc_read_dwca(make_archive(event = data.frame(
    id = c("C", "S1", "S2"), eventID = c("C", "S1", "S2"), parentEventID = c(NA, "C", "C"),
    eventDate = c("2019-05/2019-06", "2019-05-21", "2019-08-01"))))
  issues <- run_check("child_date_within_parent", dataset)
  expect_equal(issues$record_id, "S2")
})

test_that("eventdate_eurobis_compatible: a time-only interval end is a warning, a shortened date an error", {
  result <- qc_check(make_archive(event = data.frame(
    id = c("E1", "E2"), eventID = c("E1", "E2"), eventDate = c("2019-05-21T10:00/12:00", "2011-07/09"))),
    online = FALSE, checks = "eventdate_eurobis_compatible")
  issues <- qc_issues(result, check = "eventdate_eurobis_compatible")
  expect_equal(issues[record_id == "E1", severity], "warning")
  expect_equal(issues[record_id == "E1", suggestion], "2019-05-21T10:00/2019-05-21T12:00")
  expect_equal(issues[record_id == "E2", severity], "error")
  expect_equal(issues[record_id == "E2", suggestion], "2011-07/2011-09")
})
