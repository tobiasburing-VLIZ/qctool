test_that("is_iso8601 accepts the forms Darwin Core allows", {
  valid <- c("2019", "2019-05", "2019-05-21", "2019-141", "20190521", "2019-05-21T14:30", "2019-05-21T14:30Z",
             "2019-05-21T14:30:15+01:00", "2019-05-21T14:30:15.5Z", "2019-05-21/2019-05-25", "2007-11-13/15",
             "2011-07/09", "1900/1909", "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z",
             "2013-08-12 19:47:00", "2019-05-21 14:30")   # a space instead of the T: accepted by EMODnet
  expect_true(all(is_iso8601(valid)), info = paste(valid[!is_iso8601(valid)], collapse = ", "))
  range <- iso_date_range(c("2013-08-12 19:47:00", "2013-08-12 19:47:00/2013-08-14 06:00:00"))
  expect_equal(range$start, as.Date(c("2013-08-12", "2013-08-12")))
  expect_equal(range$end, as.Date(c("2013-08-12", "2013-08-14")))
})

test_that("is_iso8601 refuses other date formats and impossible dates", {
  invalid <- c("21/05/2019", "May 2019", "Jan2008", "2019-5-21", "2019-02-30", "2019-13-01", "19-05-21",
               "2019-05-21  14:30", "2019-05-21T25:00", "2019-05-21 25:00", "")
  expect_false(any(is_iso8601(invalid)), info = paste(invalid[is_iso8601(invalid)], collapse = ", "))
})

test_that("number tests are strict", {
  expect_equal(is_decimal_number(c("12", "-3.5", ".25", "1e-3", "1,5", "12 m", " 3", "")),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_whole_number(c("0", "-3", "2.0", "1,000")), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("rows_where tests each value once and never flags on an NA test result", {
  calls <- 0
  test <- function(v) {
    calls <<- calls + 1
    c(a = TRUE, b = NA, c = FALSE)[v]
  }
  expect_equal(rows_where(c("a", "b", "c", "a", NA), test), c(1L, 4L))
  expect_equal(calls, 1)
})

test_that("per_value gives the same answer as working on every value", {
  x <- rep(c("1", " 2.5", "abc", NA, "http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/"), 30000)
  expect_equal(per_value(x, toupper), toupper(x))
  expect_equal(as_number(x)[1:5], c(1, 2.5, NA, NA, NA))
  expect_equal(nerc_code(x)[5], "OCOUNT01")
  # More than 100,000 different values: no endless recursion.
  many <- as.character(seq_len(150000))
  expect_equal(as_number(many), as.numeric(many))
})
