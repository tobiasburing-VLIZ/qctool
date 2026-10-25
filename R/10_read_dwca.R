# =============================================================================
# Reading a Darwin Core Archive (zip file or unzipped folder)
#
# meta.xml is the authority. It says which file is which table, how values are
# separated and quoted, how many header lines a file has, and which column
# holds which term. Column names therefore come from meta.xml, never from the
# header row: archives from GBIF IPTs and MGnify often have no header at all.
# The header row is only kept for the check file_headers_valid.
#
# Reading never stops at the first problem. Problems are collected in
# dataset$problems and reported by the check archive_readable.
# =============================================================================

#' Read a Darwin Core Archive
#'
#' @param path Path to a DwC-A zip file or to an unzipped archive folder.
#' @return A `qc_dataset` (see `R/06_dataset.R` for its structure).
#' @export
#' @examples
#' \dontrun{
#' dataset <- qc_read_dwca("dwca-biofun_2009-v1.9.zip")
#' dataset
#' }
qc_read_dwca <- function(path) {
  dataset <- new_dataset(path, type = "dwca")

  folder <- tryCatch(archive_folder(path), error = function(e) e)
  if (inherits(folder, "error")) return(add_problem(dataset, basename(path), conditionMessage(folder)))
  if (folder$temporary) on.exit(unlink(folder$cleanup, recursive = TRUE), add = TRUE)

  meta <- tryCatch(read_meta_xml(folder$path), error = function(e) e)
  if (inherits(meta, "error")) return(add_problem(dataset, "meta.xml", conditionMessage(meta)))

  for (i in seq_len(nrow(meta$files))) {
    file_info <- meta$files[i]
    dataset <- read_data_file(dataset, folder$path, file_info, meta$fields[table == file_info$table])
  }
  dataset$core <- meta$files$table[meta$files$is_core][1]
  dataset$fields <- meta$fields
  read_eml_into(dataset, folder$path, meta$eml_file)
}

# --- Step 1: find the folder with meta.xml (unzip when needed) ----------------

archive_folder <- function(path) {
  if (dir.exists(path)) return(list(path = path, temporary = FALSE))
  if (!file.exists(path)) stop("File not found: ", path)

  refuse_too_large_unzipped(path)
  folder <- tempfile("qctool_dwca_")
  unzipped <- tryCatch(utils::unzip(path, exdir = folder), warning = function(w) character(), error = function(e) character())
  if (!length(unzipped)) stop("Could not unzip ", basename(path), ": not a valid zip file")

  # Some zips put the archive inside one sub-folder.
  if (!file.exists(file.path(folder, "meta.xml"))) {
    inner <- dirname(list.files(folder, pattern = "^meta\\.xml$", recursive = TRUE, full.names = TRUE))
    if (length(inner) == 1) return(list(path = inner, temporary = TRUE, cleanup = folder))
  }
  list(path = folder, temporary = TRUE, cleanup = folder)
}

# Stop before unzipping when the archive would be larger than the option
# qctool.max_unzipped_mb (R/08_options.R; default: no limit). The sizes come
# from the zip's own list of files, so nothing is unpacked to find out.
refuse_too_large_unzipped <- function(path) {
  limit_mb <- qc_option("qctool.max_unzipped_mb")
  if (!is.finite(limit_mb)) return(invisible(TRUE))
  listed <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  if (is.null(listed)) return(invisible(TRUE))     # not a zip: reported when unzipping
  size_mb <- sum(as.numeric(listed$Length)) / 1024^2
  if (size_mb > limit_mb) {
    stop(sprintf("Unzipped, the archive would be %.0f MB, more than the limit of %s MB (option qctool.max_unzipped_mb)",
                 size_mb, format(limit_mb, big.mark = ",")))
  }
  invisible(TRUE)
}

# --- Step 2: read meta.xml ---------------------------------------------------------

# Table names used everywhere in the tool, by row type.
table_names_by_row_type <- c(
  "http://rs.tdwg.org/dwc/terms/Event"                       = "event",
  "http://rs.tdwg.org/dwc/terms/Occurrence"                  = "occurrence",
  "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" = "emof",
  "http://rs.tdwg.org/dwc/terms/MeasurementOrFact"           = "mof",
  "http://rs.gbif.org/terms/1.0/DNADerivedData"              = "dna"
)

read_meta_xml <- function(folder) {
  meta_path <- file.path(folder, "meta.xml")
  if (!file.exists(meta_path)) {
    stop("meta.xml is missing, so the columns cannot be identified. Files found: ",
         paste(list.files(folder), collapse = ", "))
  }
  doc <- tryCatch(xml2::read_xml(meta_path), error = function(e) stop("meta.xml is not valid XML: ", conditionMessage(e)))
  xml2::xml_ns_strip(doc)

  nodes <- c(xml2::xml_find_all(doc, "/archive/core"), xml2::xml_find_all(doc, "/archive/extension"))
  if (!length(nodes)) stop("meta.xml describes no core and no extension")

  files <- rbindlist(lapply(nodes, meta_file_info))
  files[, table := make.unique(table, sep = "_")]

  fields <- rbindlist(lapply(seq_along(nodes), function(i) {
    meta_field_info(nodes[[i]], files$table[i])
  }))

  list(files = files, fields = fields, eml_file = xml2::xml_attr(xml2::xml_find_first(doc, "/archive"), "metadata"))
}

# One row per <core> or <extension>: file, separators, header lines, id column.
meta_file_info <- function(node) {
  attribute <- function(name, default) {
    value <- xml2::xml_attr(node, name)
    if (is.na(value)) default else value
  }
  row_type <- attribute("rowType", NA_character_)
  table_name <- unname(table_names_by_row_type[row_type])
  if (is.na(table_name)) table_name <- tolower(sub("^.*[/#]", "", row_type))

  id_node <- xml2::xml_find_first(node, "id|coreid")
  data.table(
    table        = table_name,
    file         = xml2::xml_text(xml2::xml_find_first(node, "files/location")),
    row_type     = row_type,
    is_core      = xml2::xml_name(node) == "core",
    delimiter    = decode_meta_separator(attribute("fieldsTerminatedBy", ",")),
    quote        = attribute("fieldsEnclosedBy", "\""),
    header_lines = as.integer(attribute("ignoreHeaderLines", "0")),
    encoding     = attribute("encoding", "UTF-8"),
    id_index     = as.integer(xml2::xml_attr(id_node, "index"))
  )
}

# One row per <field>: term IRI, column index (NA when only a default value is given).
meta_field_info <- function(node, table_name) {
  field_nodes <- xml2::xml_find_all(node, "field")
  data.table(
    table    = table_name,
    term_iri = xml2::xml_attr(field_nodes, "term"),
    index    = as.integer(xml2::xml_attr(field_nodes, "index")),
    default  = xml2::xml_attr(field_nodes, "default")
  )[, column := term_name_from_iri(term_iri)]
}

# meta.xml writes a tab as "\t" (two characters).
decode_meta_separator <- function(value) {
  switch(value, "\\t" = "\t", "\\n" = "\n", "\\r\\n" = "\r\n", value)
}

# --- Step 3: read one data file --------------------------------------------------

read_data_file <- function(dataset, folder, file_info, fields) {
  path <- file.path(folder, file_info$file)
  if (is.na(file_info$file) || !file.exists(path)) {
    return(add_problem(dataset, file_info$file, "listed in meta.xml but missing from the archive"))
  }

  header_names <- if (file_info$header_lines > 0) utf8_safe(read_header_names(path, file_info)) else character()

  # Collect fread's warnings (e.g. "stopped early") as reading problems.
  read_warnings <- character()
  data <- tryCatch(
    withCallingHandlers(
      fread(path, sep = file_info$delimiter, quote = file_info$quote, header = FALSE,
            skip = file_info$header_lines, colClasses = "character",
            na.strings = "",          # only empty cells are missing; "NA" stays text
            strip.white = FALSE,      # keep spaces, the checks need to see them
            fill = TRUE, blank.lines.skip = TRUE, showProgress = FALSE,
            encoding = fread_encoding(file_info$encoding)),
      warning = function(w) {
        read_warnings <<- c(read_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  for (message in read_warnings) dataset <- add_problem(dataset, file_info$file, paste("While reading:", message))

  if (inherits(data, "error")) {
    # A file with only its header row (or nothing at all) is an empty table,
    # reported by table_not_empty. Anything else could not be read.
    if (file.size(path) == 0 || grepl("only has|empty", conditionMessage(data))) {
      data <- data.table()
    } else {
      return(add_problem(dataset, file_info$file, paste("Could not be read:", conditionMessage(data))))
    }
  }

  if (nzchar(file_info$quote)) data <- unescape_doubled_quotes(data, file_info$quote)
  data <- name_columns(data, fields, file_info$id_index)
  for (problem in attr(data, "problems")) dataset <- add_problem(dataset, file_info$file, problem)

  # Text that is not valid UTF-8 in a file meta.xml calls UTF-8 (e.g. saved as
  # Windows-1252) would make checks fail: repair it now and remember where it
  # was (text_encoding_valid). A file declared as Latin-1 is read as Latin-1.
  repaired <- if (fread_encoding(file_info$encoding) == "UTF-8") repair_invalid_text(data) else list(data = data, invalid = data.table())
  data <- repaired$data
  if (nrow(repaired$invalid)) {
    dataset$invalid_text[[file_info$table]] <- repaired$invalid[, .(column, row)]
    dataset <- add_note(dataset, invalid_text_note(repaired$invalid, file_info$file))
  }
  data[, .line := file_info$header_lines + seq_len(.N)]

  dataset$tables[[file_info$table]] <- data
  dataset$files <- rbind(dataset$files, data.table(
    table = file_info$table, file = file_info$file, row_type = file_info$row_type, is_core = file_info$is_core,
    delimiter = file_info$delimiter, quote = file_info$quote, header_lines = file_info$header_lines,
    encoding = file_info$encoding, id_index = file_info$id_index, records = nrow(data),
    columns = length(data_columns(data)), header_names = list(header_names)))
  dataset
}

# Give each column its term name (from meta.xml), add .coreid, add default values.
# Columns that meta.xml does not map get the name column_<number>.
name_columns <- function(data, fields, id_index) {
  problems <- character()
  if (ncol(data) == 0) {
    # Empty file: build an empty table with as many columns as meta.xml describes.
    n_columns <- max(c(fields$index, id_index, -1L), na.rm = TRUE) + 1L
    data <- as.data.table(rep(list(character()), n_columns))
  }
  column_names <- paste0("column_", seq_len(ncol(data)))

  mapped <- fields[!is.na(index)]
  outside <- mapped[index + 1 > ncol(data)]
  for (i in seq_len(nrow(outside))) {
    problems <- c(problems, sprintf("meta.xml maps %s to column %d, but the file has only %d columns",
                                    outside$column[i], outside$index[i] + 1, ncol(data)))
  }
  mapped <- mapped[index + 1 <= ncol(data)]
  doubled <- mapped$column[duplicated(mapped$column)]
  if (length(doubled)) problems <- c(problems, paste("meta.xml maps the same term twice:", paste(unique(doubled), collapse = ", ")))
  column_names[mapped$index + 1] <- make.unique(mapped$column, sep = "_")
  setnames(data, column_names)

  # The link to the core. Copy it when the id column is also a term column (e.g. eventID).
  if (!is.na(id_index) && id_index + 1 <= ncol(data)) {
    id_column <- column_names[id_index + 1]
    if (grepl("^column_", id_column)) setnames(data, id_column, ".coreid") else data[, .coreid := get(id_column)]
  } else {
    if (!is.na(id_index)) problems <- c(problems, sprintf("meta.xml says the id is in column %d, but the file has only %d columns", id_index + 1, ncol(data)))
    data[, .coreid := NA_character_]
  }

  # Terms that meta.xml maps to columns the file does not have: add as empty.
  for (term in setdiff(outside$column, names(data))) set(data, j = term, value = NA_character_)

  # Default values: a constant for the whole column, or the fill-in for empty cells.
  with_default <- fields[!is.na(default)]
  for (i in seq_len(nrow(with_default))) {
    term <- with_default$column[i]
    if (term %in% names(data)) {
      set(data, i = which(is.na(data[[term]])), j = term, value = with_default$default[i])
    } else {
      set(data, j = term, value = with_default$default[i])
    }
  }

  setattr(data, "problems", problems)
  data
}

# In a quoted CSV field a quote character is written twice: "{""k"":""v""}" means
# {"k":"v"}. fread keeps both quotes, so turn each pair back into one.
unescape_doubled_quotes <- function(data, quote) {
  doubled <- strrep(quote, 2)
  for (column in names(data)) {
    values <- data[[column]]
    rows <- which(grepl(doubled, values, fixed = TRUE, useBytes = TRUE))
    if (length(rows)) set(data, i = rows, j = column, value = gsub(doubled, quote, values[rows], fixed = TRUE, useBytes = TRUE))
  }
  data
}

# The first line of a file, split into column names (only used by file_headers_valid).
read_header_names <- function(path, file_info) {
  connection <- file(path, encoding = if (fread_encoding(file_info$encoding) == "Latin-1") "latin1" else "UTF-8")
  on.exit(close(connection))
  first_line <- readLines(connection, n = 1, warn = FALSE)
  if (!length(first_line)) return(character())
  first_line <- sub("^﻿", "", first_line)  # byte order mark
  names <- strsplit(first_line, file_info$delimiter, fixed = TRUE)[[1]]
  if (nzchar(file_info$quote)) names <- gsub(file_info$quote, "", names, fixed = TRUE)
  names
}

fread_encoding <- function(meta_encoding) {
  if (toupper(meta_encoding) %in% c("ISO-8859-1", "LATIN1", "LATIN-1", "WINDOWS-1252", "CP1252")) "Latin-1" else "UTF-8"
}
