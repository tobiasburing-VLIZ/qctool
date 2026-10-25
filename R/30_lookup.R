# =============================================================================
# Online lookups: the part all services share (WoRMS, NERC, OBIS xylookup)
#
#   values -> each different value once -> known in this run? -> disk cache -> internet
#
# Disk cache: one file per service (worms_records.rds, nerc_terms.rds, ...) in
#   tools::R_user_dir("qctool", "cache"), or options(qctool.cache_dir = "...").
#   Each entry keeps the date it was fetched. When online, entries older than
#   options(qctool.cache_days) (default 30) are fetched again.
# Offline: qc_check(online = FALSE), or no internet connection. Then only the
#   cache is used (old entries too). A check that needs values which are not
#   in the cache is "not performed" - never an error in the data.
# A service that does not answer after 3 tries: "not performed" as well, and
#   the other checks of this run do not try that service again. The answers
#   that did come are cached, so the next run only asks for the rest.
#
# Service files: 31_lookup_worms.R, 32_lookup_nerc.R, 33_lookup_xy.R.
# =============================================================================

cache_dir <- function() qc_option("qctool.cache_dir")     # options: R/08_options.R
cache_days <- function() qc_option("qctool.cache_days")
cache_file <- function(service) file.path(cache_dir(), paste0(service, ".rds"))

# Is this run allowed to use the internet (qc_check(online = ), default
# options(qctool.online = TRUE)), and is there a connection?
online <- function(dataset) {
  if (!online_allowed(dataset)) return(FALSE)
  cached(dataset, "internet", internet_available())
}

online_allowed <- function(dataset) {
  if (is.null(dataset$online)) isTRUE(qc_option("qctool.online")) else isTRUE(dataset$online)
}

internet_available <- function() curl::has_internet()

offline_reason <- function(dataset) {
  if (!online_allowed(dataset)) "the check ran offline (qc_check(online = FALSE))" else "there is no internet connection"
}

# -----------------------------------------------------------------------------
# lookup_values(): the result for each different key, as a data.table with the
# columns query (the looked-up value), found, fetched and whatever the service gives.
#   service     name of the cache file, e.g. "worms_records"
#   fetch       function(batches) -> data.table, one row per key (found TRUE/FALSE);
#               keys whose request got no answer are left out (fetch_and_parse)
#   batch_size  keys per internet request
#   what        words for messages, e.g. "AphiaIDs"
# Keys that cannot be looked up (offline, or no answer) make the check "not
# performed". The answers that did come are cached all the same, so the next
# run only asks for the rest.
# -----------------------------------------------------------------------------
lookup_values <- function(dataset, service, keys, fetch, batch_size, what) {
  keys <- without_na(unique(keys))
  memory_key <- paste0("lookup_", service)
  known <- cached(dataset, memory_key, data.table(query = character(), found = logical()))
  to_find <- setdiff(keys, known$query)
  if (!length(to_find)) return(known[match(keys, known$query)])

  stored <- cache_read(service)
  recent <- if (online(dataset)) stored$fetched >= Sys.Date() - cache_days() else rep(TRUE, nrow(stored))
  from_disk <- stored[recent & stored$query %chin% to_find]
  to_fetch <- setdiff(to_find, from_disk$query)
  fetched <- if (length(to_fetch)) fetch_in_batches(dataset, service, to_fetch, fetch, batch_size) else NULL
  if (!is.null(fetched) && nrow(fetched)) cache_write(service, fetched)

  known <- rbindlist(list(known, from_disk, fetched), fill = TRUE)
  assign(memory_key, known, envir = dataset$cache)
  log_lookup(dataset, service, what, from_cache = nrow(from_disk), fetched = if (is.null(fetched)) 0L else nrow(fetched))

  not_found <- setdiff(keys, known$query)
  if (length(not_found)) {
    why <- if (!online(dataset)) offline_reason(dataset) else get0(paste0("failed_", service), envir = dataset$cache, ifnotfound = "no answer")
    not_performed(sprintf("%s of %s %s could not be looked up: %s", format(length(not_found), big.mark = ","),
                          format(length(keys), big.mark = ","), what, why))
  }
  known[match(keys, known$query)]
}

# NULL when offline, or when the service already failed in this run (it is
# not tried again). A failure is remembered for the other checks of the run.
fetch_in_batches <- function(dataset, service, keys, fetch, batch_size) {
  failure_key <- paste0("failed_", service)
  if (!online(dataset) || exists(failure_key, envir = dataset$cache, inherits = FALSE)) return(NULL)
  batches <- split(keys, ceiling(seq_along(keys) / batch_size))
  fetched <- tryCatch(fetch(batches), qc_http_failed = function(condition) condition)
  if (inherits(fetched, "condition")) {
    assign(failure_key, conditionMessage(fetched), envir = dataset$cache)
    return(NULL)
  }
  failure <- attr(fetched, "failure")
  if (!is.null(failure)) {
    assign(failure_key, failure, envir = dataset$cache)
    setattr(fetched, "failure", NULL)
  }
  if (nrow(fetched)) fetched[, fetched := Sys.Date()]
  fetched
}

# Send the requests and parse each answer with parse(batch, response). Requests
# that got no answer are left out, with the reason in the attribute "failure":
# the answers that did come are still used.
fetch_and_parse <- function(batches, requests, service, parse) {
  responses <- http_fetch(requests, service)
  ok <- vapply(responses, answered, logical(1))
  result <- rbindlist(Map(parse, batches[ok], responses[ok]), fill = TRUE)
  if (!all(ok)) {
    failed <- responses[[which(!ok)[1]]]
    setattr(result, "failure", sprintf("%s did not answer (%s)", service,
                                       if (!is.null(failed$error)) failed$error else paste("HTTP", failed$status_code)))
  }
  result
}

# What was looked up in this run, for the result and the report.
log_lookup <- function(dataset, service, what, from_cache, fetched) {
  entry <- data.table(service = service, what = what, from_cache = from_cache, from_internet = fetched)
  assign("lookup_log", rbind(cached(dataset, "lookup_log", NULL), entry), envir = dataset$cache)
}

lookup_labels <- c(worms_records = "WoRMS", worms_names = "WoRMS", nerc_terms = "NERC", xylookup = "OBIS xylookup")

lookup_summary <- function(dataset) {
  log <- cached(dataset, "lookup_log", NULL)
  if (is.null(log) || !nrow(log)) return(NULL)
  log[, .(from_cache = sum(from_cache), from_internet = sum(from_internet)), by = .(service, what)]
}

# --- Disk cache ----------------------------------------------------------------------

cache_read <- function(service) {
  empty <- data.table(query = character(), found = logical(), fetched = as.Date(character()))
  file <- cache_file(service)
  if (!file.exists(file)) return(empty)
  tryCatch(readRDS(file), error = function(e) empty)   # a damaged cache is simply refilled
}

# Add new entries. Written to a temporary file first and then renamed, so two
# users of a shared app never leave half a file behind.
cache_write <- function(service, new) {
  dir.create(cache_dir(), recursive = TRUE, showWarnings = FALSE)
  stored <- cache_read(service)
  combined <- rbindlist(list(stored[!stored$query %chin% new$query], new), fill = TRUE)
  temporary <- tempfile(tmpdir = cache_dir(), fileext = ".tmp")
  saveRDS(combined, temporary)
  if (!suppressWarnings(file.rename(temporary, cache_file(service)))) {
    file.copy(temporary, cache_file(service), overwrite = TRUE)
    unlink(temporary)
  }
  invisible(TRUE)
}

#' Empty the cache of online lookups
#'
#' Removes the stored WoRMS, NERC and OBIS answers, so the next check fetches
#' them again.
#' @param service Optional: only this service ("worms_records", "worms_names",
#'   "nerc_terms", "xylookup").
#' @return The removed files, invisibly.
#' @export
qc_clear_cache <- function(service = NULL) {
  files <- if (is.null(service)) list.files(cache_dir(), pattern = "\\.rds$", full.names = TRUE) else cache_file(service)
  files <- files[file.exists(files)]
  unlink(files)
  invisible(files)
}

# --- Internet ------------------------------------------------------------------------

# Send all requests (up to 4 at a time) and return the responses in the same
# order. A request is tried 3 times; network errors, HTTP 5xx and 429 count as
# "no answer" (see answered()); such responses are returned as they are.
# A request: list(url = , body = NULL, content_type = NULL, accept = NULL,
# timeout = NULL (seconds; default options(qctool.timeout), 60)).
http_fetch <- function(requests, service, tries = 3) {
  responses <- vector("list", length(requests))
  todo <- seq_along(requests)
  for (attempt in seq_len(tries)) {
    if (attempt > 1) Sys.sleep(2 * (attempt - 1))
    pool <- curl::new_pool(host_con = 4)
    for (i in todo) local({
      index <- i
      curl::multi_add(request_handle(requests[[index]]), pool = pool,
                      done = function(response) responses[[index]] <<- response,
                      fail = function(message) responses[[index]] <<- list(status_code = NA_integer_, error = message))
    })
    curl::multi_run(pool = pool)
    todo <- todo[!vapply(responses[todo], answered, logical(1))]
    if (!length(todo)) break
  }
  responses
}

answered <- function(response) {
  !is.null(response) && !is.na(response$status_code) && response$status_code < 500 && response$status_code != 429
}

request_handle <- function(request) {
  timeout <- if (is.null(request$timeout)) qc_option("qctool.timeout") else request$timeout
  handle <- curl::new_handle(url = request$url, timeout = timeout, useragent = "qctool (QC Tool Beta, EMODnet Biology)")
  if (!is.null(request$accept)) curl::handle_setheaders(handle, Accept = request$accept)
  if (!is.null(request$body)) {
    curl::handle_setopt(handle, post = TRUE, postfields = request$body)
    curl::handle_setheaders(handle, "Content-Type" = request$content_type)
  }
  handle
}

# The text of a response; stops with qc_http_failed for an unexpected status.
response_text <- function(response, service, ok = 200) {
  if (!response$status_code %in% ok) {
    stop(structure(class = c("qc_http_failed", "error", "condition"),
                   list(message = sprintf("%s answered with HTTP %s", service, response$status_code), call = NULL)))
  }
  rawToChar(response$content)
}

# A field of a parsed JSON record, or NA when it is missing or null.
json_field <- function(record, name, missing = NA) {
  value <- record[[name]]
  if (is.null(value)) missing else value
}
