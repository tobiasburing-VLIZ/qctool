# =============================================================================
# Reading a dataset from the internet: an IPT, GBIF or OBIS address, a link to
# a Darwin Core Archive (zip), or a link to a table (CSV, TXT, TSV or Excel)
#
#   https://ipt.vliz.be/eurobis/resource?r=fifd_biodiversity_data   IPT resource page
#   https://ipt.vliz.be/eurobis/archive.do?r=fifd_biodiversity_data IPT archive
#   https://www.gbif.org/dataset/<uuid>                              GBIF dataset page
#   https://obis.org/dataset/<uuid>                                  OBIS dataset page
#   https://.../some_archive.zip                                     any zip link
#   https://.../my_table.csv  (.txt, .tsv, .xlsx, .xls)              any table link
#
# 1. download_dataset(): the address of the file is found (for GBIF and OBIS
#    through their API), its size is checked BEFORE downloading (some GBIF
#    datasets are many GB), and it is downloaded to a temporary folder. What it
#    is (archive or table) is decided by its content, not by the address:
#    a zip with meta.xml is an archive, a zip with xl/workbook.xml is Excel,
#    plain text is a table.
# 2. read_downloaded(): read like a local archive or table.
# The app does the two steps apart, so a table can get its column mapping in
# between (R/61_app_load.R). Options (size limit, server safety): R/08_options.R.
# =============================================================================

#' Read a dataset from an IPT, GBIF or OBIS address, or from a link to a file
#'
#' @param url Address of an IPT resource, a GBIF or OBIS dataset page, a zip
#'   file, or a table (.csv, .txt, .tsv, .xlsx, .xls).
#' @param max_size_mb Refuse files larger than this (in MB) before or while
#'   downloading. Default: the option `qctool.max_download_mb` (2000).
#' @param mapping,sheet,table_type Tables only, see [qc_read_table()].
#' @return A `qc_dataset`.
#' @export
qc_read_url <- function(url, max_size_mb = qc_option("qctool.max_download_mb"),
                        mapping = NULL, sheet = NULL, table_type = "occurrence") {
  download <- download_dataset(url, max_size_mb)
  on.exit(unlink(download$folder, recursive = TRUE), add = TRUE)
  read_downloaded(download, mapping = mapping, sheet = sheet, table_type = table_type)
}

is_url <- function(x) grepl("^https?://", x, ignore.case = TRUE)

# Kinds of downloaded files that are tables (the others: "archive").
table_kinds <- c("csv", "txt", "tsv", "xlsx", "xls")

# Download the dataset behind an address into a new temporary folder.
# Returns a list: url (as given), archive_url (what was downloaded), folder,
# file (with a readable name and the right extension), kind ("archive" or one
# of table_kinds) and name (the file name shown in the report).
download_dataset <- function(url, max_size_mb = qc_option("qctool.max_download_mb")) {
  url <- trimws(url)
  if (!is_url(url)) stop("A web address must start with http:// or https://: ", url, call. = FALSE)
  refuse_private_address(url)
  archive_url <- archive_url_for(url)
  refuse_private_address(archive_url)
  size_mb <- remote_size_mb(archive_url)
  if (!is.na(size_mb) && size_mb > max_size_mb) stop(too_large_message(size_mb, max_size_mb), call. = FALSE)

  folder <- tempfile("qctool_download_")
  dir.create(folder)
  downloaded <- file.path(folder, "download")
  final_url <- tryCatch(download_with_limit(archive_url, downloaded, max_size_mb),
                        error = function(e) {
                          unlink(folder, recursive = TRUE)
                          stop(e)
                        })
  refuse_private_address(final_url, after_download = folder)   # where redirects ended up

  kind <- tryCatch(downloaded_kind(downloaded, archive_url), error = function(e) {
    unlink(folder, recursive = TRUE)
    stop(e)
  })
  name <- downloaded_name(archive_url, kind)
  file <- file.path(folder, name)
  file.rename(downloaded, file)
  list(url = url, archive_url = archive_url, folder = folder, file = file, kind = kind, name = name)
}

# Read a downloaded dataset (see download_dataset) and note where it came from.
read_downloaded <- function(download, mapping = NULL, sheet = NULL, table_type = "occurrence") {
  if (download$kind %in% table_kinds) {
    dataset <- qc_read_table(download$file, mapping = mapping, sheet = sheet, table_type = table_type)
    # source$type stays "table": a table has no meta.xml and no metadata, wherever it comes from.
  } else {
    dataset <- qc_read_dwca(download$file)
    dataset$source$type <- "url"
    if (is.na(dataset$info$ipt_url) && grepl("archive\\.do\\?r=", download$archive_url)) {
      dataset$info$ipt_url <- sub("archive\\.do\\?", "resource?", download$archive_url)
    }
  }
  dataset$source$path <- download$url
  dataset$source$name <- download$name
  dataset$source$archive_url <- download$archive_url
  dataset
}

# The address of the file behind an IPT, GBIF or OBIS address.
archive_url_for <- function(url) {
  url <- trimws(url)
  if (grepl("/archive\\.do\\?r=", url)) return(url)
  if (grepl("/resource(\\.do)?\\?r=", url)) return(sub("/resource(\\.do)?\\?r=", "/archive.do?r=", url))

  gbif_uuid <- regmatches(url, regexpr("(?<=gbif\\.org/dataset/)[0-9a-f-]{36}", url, perl = TRUE))
  if (length(gbif_uuid)) {
    endpoints <- read_json_from(sprintf("https://api.gbif.org/v1/dataset/%s/endpoint", gbif_uuid))
    archive <- endpoints$url[endpoints$type == "DWC_ARCHIVE"]
    if (!length(archive)) stop("GBIF lists no Darwin Core Archive for dataset ", gbif_uuid, call. = FALSE)
    return(archive[1])
  }

  obis_uuid <- regmatches(url, regexpr("(?<=obis\\.org/dataset/)[0-9a-f-]{36}", url, perl = TRUE))
  if (length(obis_uuid)) {
    info <- read_json_from(sprintf("https://api.obis.org/v3/dataset/%s", obis_uuid))
    archive <- info$results$archive
    if (!length(archive) || is.na(archive[1])) stop("OBIS lists no archive for dataset ", obis_uuid, call. = FALSE)
    return(archive[1])
  }

  url   # anything else: assume it is the file itself
}

read_json_from <- function(url) {
  response <- tryCatch(curl::curl_fetch_memory(url), error = function(e) e)
  if (inherits(response, "error")) {
    stop("Could not reach ", url, ": ", conditionMessage(response), "\nIs there an internet connection?", call. = FALSE)
  }
  if (response$status_code != 200) stop(url, " answered with status ", response$status_code, call. = FALSE)
  jsonlite::fromJSON(rawToChar(response$content))
}

# Download in pieces and stop as soon as the size limit is passed. Needed because
# many servers (IPTs) do not say the size in advance. Returns the address the
# file finally came from (after redirects).
download_with_limit <- function(url, file, max_size_mb) {
  connection <- file(file, open = "wb")
  on.exit(close(connection), add = TRUE)
  written <- 0
  too_large <- FALSE
  response <- tryCatch(
    curl::curl_fetch_stream(url, function(piece) {
      written <<- written + length(piece)
      if (written > max_size_mb * 1024^2) {
        too_large <<- TRUE
        stop("size limit")
      }
      writeBin(piece, connection)
    }, handle = curl::new_handle(followlocation = TRUE)),
    error = function(e) e)

  if (too_large) stop(too_large_message(NA, max_size_mb), call. = FALSE)
  if (inherits(response, "error")) {
    stop("Could not download ", url, ": ", conditionMessage(response),
         "\nIs there an internet connection, and is the resource public?", call. = FALSE)
  }
  if (response$status_code != 200) {
    stop("Could not download ", url, ": the server answered with status ", response$status_code,
         ". Is the resource public?", call. = FALSE)
  }
  if (is.null(response$url)) url else response$url
}

too_large_message <- function(size_mb, max_size_mb) {
  size <- if (is.na(size_mb)) "larger than" else sprintf("%.0f MB, more than", size_mb)
  sprintf("The file is %s max_size_mb = %s. Download it yourself, or raise the limit (option qctool.max_download_mb).",
          size, max_size_mb)
}

# Size of a file on the internet in MB, from the Content-Length header (NA when unknown).
remote_size_mb <- function(url) {
  handle <- curl::new_handle(nobody = TRUE, followlocation = TRUE)
  response <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) NULL)
  if (is.null(response)) return(NA_real_)
  headers <- curl::parse_headers_list(response$headers)
  bytes <- suppressWarnings(as.numeric(headers[["content-length"]]))
  if (length(bytes) == 1 && !is.na(bytes)) bytes / 1024^2 else NA_real_
}

# --- What was downloaded ---------------------------------------------------------------

# "archive", "xlsx", "xls", "tsv", "txt" or "csv", from the first bytes of the
# file. Addresses that can only give an archive (IPT archive.do, .zip) are
# always read as one: if the server sent something else (an error page),
# archive_readable reports it, as for any broken archive.
downloaded_kind <- function(file, address) {
  start <- readBin(file, "raw", 8)
  if (length(start) >= 4 && identical(start[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {    # zip
    entries <- tryCatch(utils::unzip(file, list = TRUE)$Name, error = function(e) character())
    if (!any(basename(entries) == "meta.xml") && any(grepl("(^|/)xl/workbook\\.xml$", entries))) return("xlsx")
    return("archive")
  }
  if (length(start) >= 4 && identical(start[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) return("xls")   # old Excel
  if (grepl("archive\\.do\\?|\\.zip($|\\?)", address, ignore.case = TRUE)) return("archive")

  first_bytes <- readBin(file, "raw", 2000)
  beginning <- rawToChar(first_bytes[first_bytes != as.raw(0)])
  if (grepl("^\\s*<(!doctype\\s+html|html)", beginning, ignore.case = TRUE, useBytes = TRUE)) {
    stop("The address gave a web page, not a dataset (a Darwin Core Archive, or a CSV, TXT or Excel table). ",
         "Give the address of the file itself, or of an IPT resource.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(sub("[?#].*$", "", address)))
  if (extension %in% c("tsv", "txt")) extension else "csv"
}

# A readable file name with the right extension: the IPT short name, or the
# last part of the address.
downloaded_name <- function(address, kind) {
  name <- archive_name(address)
  name <- gsub("[^A-Za-z0-9._-]+", "_", tryCatch(utils::URLdecode(name), error = function(e) name))
  if (!nzchar(name) || name %in% c(".", "_")) name <- "download"
  extension <- if (kind == "archive") "zip" else kind
  if (tolower(tools::file_ext(name)) != extension) name <- paste0(tools::file_path_sans_ext(name), ".", extension)
  name
}

# A readable name for the download: the IPT short name or the file name.
archive_name <- function(archive_url) {
  short_name <- regmatches(archive_url, regexpr("(?<=[?&]r=)[^&]+", archive_url, perl = TRUE))
  if (length(short_name)) return(paste0(short_name, ".zip"))
  basename(sub("\\?.*$", "", archive_url))
}

# --- Safety on a server (option qctool.block_private_addresses) ---------------------------

# With the option on, refuse addresses on the server's own network: someone
# could otherwise make the server fetch internal pages. Off by default (a
# laptop or an internal server may need an internal IPT). On a public server
# IT should ALSO block such traffic in the network (an egress rule): a public
# address can redirect to an internal one before this check sees it.
refuse_private_address <- function(url, after_download = NULL) {
  if (!isTRUE(qc_option("qctool.block_private_addresses"))) return(invisible(TRUE))
  host <- url_host(url)
  addresses <- if (grepl("^[0-9.]+$|:", host)) host else tryCatch(curl::nslookup(host, multiple = TRUE), error = function(e) character())
  if (tolower(host) == "localhost" || any(is_private_address(addresses))) {
    if (!is.null(after_download)) unlink(after_download, recursive = TRUE)
    stop("This address is on a private network and cannot be used here (option qctool.block_private_addresses): ", url, call. = FALSE)
  }
  invisible(TRUE)
}

url_host <- function(url) {
  host <- sub("^[A-Za-z]+://([^/?#@]*@)?(\\[[^]]+\\]|[^/:?#]+).*$", "\\2", url)
  gsub("^\\[|\\]$", "", host)
}

# Loopback, private and link-local addresses (IPv4 and IPv6).
is_private_address <- function(ip) {
  ip <- tolower(ip)
  grepl("^(127\\.|10\\.|192\\.168\\.|169\\.254\\.|0\\.)", ip) |
    grepl("^172\\.(1[6-9]|2[0-9]|3[01])\\.", ip) |
    ip %in% c("::1", "::") | grepl("^(fc|fd|fe8|fe9|fea|feb)", ip)
}
