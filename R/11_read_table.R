# =============================================================================
# Reading a simple table (CSV, TXT, TSV or Excel) that is not a DwC archive
#
#   1. read all values as text (like the archive reader)
#      - CSV/TXT: separator detected automatically (comma, semicolon, tab, ...)
#      - Excel: each cell is read with its own type, then turned into text:
#        date cells become ISO 8601 (2019-05-21, or 2019-05-21T14:30:00),
#        numbers keep the value as typed (43.55859, not 43.558590000000002)
#   2. map the columns to Darwin Core terms (R/12_map_columns.R)
#   3. build the same dataset object as for an archive: one table (occurrence,
#      unless you choose otherwise), columns named by term, .line = the line in
#      the file (for Excel: the Excel row number)
#
# Columns without a term are kept (column_N) and reported by file_headers_valid.
# =============================================================================

#' Read a CSV, TXT or Excel table as a dataset
#'
#' @param path Path to a .csv, .txt, .tsv, .xlsx or .xls file.
#' @param mapping Optional own column mapping, e.g. `c(Date = "eventDate", Day = NA)`.
#'   Other columns get a proposal from [qc_suggest_mapping()].
#' @param sheet Excel only: sheet name or number (default: the first sheet).
#' @param table_type "occurrence" (default), "event" or "emof": what one row of the table is.
#' @return A `qc_dataset`; `dataset$mapping` shows which column became which term.
#' @export
qc_read_table <- function(path, mapping = NULL, sheet = NULL, table_type = "occurrence") {
  table_type <- match.arg(table_type, c("occurrence", "event", "emof"))
  dataset <- new_dataset(path, type = "table")
  if (!file.exists(path)) return(add_problem(dataset, basename(path), "File not found"))

  is_excel <- grepl("\\.xlsx?$", path, ignore.case = TRUE)
  raw <- tryCatch(if (is_excel) read_excel_as_text(path, sheet) else read_text_table(path), error = function(e) e)
  if (inherits(raw, "error")) {
    return(add_problem(dataset, basename(path), paste("Could not be read:", conditionMessage(raw))))
  }
  for (note in attr(raw, "notes")) dataset <- add_note(dataset, note)

  original_names <- copy(names(raw))   # copy: setnames() below changes names(raw) in place
  mapping_table <- qc_suggest_mapping(original_names, mapping)
  mapping_table[, position := seq_len(.N)]
  terms <- term_table()

  # Column names: the term where there is one, column_<n> for the others.
  new_names <- ifelse(is.na(mapping_table$term), paste0("column_", mapping_table$position), mapping_table$term)
  invalid <- attr(raw, "invalid_text")   # values read as Windows-1252 (read_text_table)
  setattr(raw, "invalid_text", NULL)
  setnames(raw, new_names)
  raw[, .coreid := NA_character_]
  raw[, .line := seq_len(.N) + 1L]   # line 1 (Excel row 1) is the header

  mapped <- mapping_table[!is.na(term)]
  file_label <- if (is_excel) sprintf("%s [%s]", basename(path), attr(raw, "sheet")) else basename(path)
  if (!is.null(invalid) && nrow(invalid)) {
    dataset$invalid_text[[table_type]] <- data.table(column = new_names[invalid$position], row = invalid$row)
  }
  dataset$tables[[table_type]] <- raw
  dataset$core <- table_type
  dataset$mapping <- mapping_table[, .(column, term, how, note, position)]
  dataset$fields <- data.table(table = table_type, column = mapped$term,
                               term_iri = terms$term_iri[match(mapped$term, terms$term)],
                               index = mapped$position - 1L, default = NA_character_)
  dataset$files <- data.table(
    table = table_type, file = file_label, row_type = names(table_names_by_row_type)[table_names_by_row_type == table_type],
    is_core = TRUE, delimiter = if (is_excel) NA_character_ else attr(raw, "delimiter"), quote = "\"",
    header_lines = 1L, encoding = "UTF-8", id_index = NA_integer_, records = nrow(raw),
    columns = length(data_columns(raw)), header_names = list(original_names))
  dataset$info$title <- tools::file_path_sans_ext(basename(path))
  dataset
}

# A note about how the data was read (not a problem), e.g. Excel dates converted.
add_note <- function(dataset, message) {
  dataset$notes <- c(dataset$notes, message)
  dataset
}

read_text_table <- function(path) {
  data <- fread(path, sep = "auto", header = TRUE, colClasses = "character", na.strings = "",
                strip.white = FALSE, fill = TRUE, check.names = FALSE, encoding = "UTF-8", showProgress = FALSE)
  # A CSV saved by Excel on Windows is often Windows-1252: repair that text now
  # (the checks need valid UTF-8) and remember where it was (text_encoding_valid).
  repaired <- repair_invalid_text(data)
  data <- repaired$data
  setattr(data, "delimiter", detect_delimiter(path))
  if (nrow(repaired$invalid)) {
    setattr(data, "invalid_text", repaired$invalid)
    setattr(data, "notes", invalid_text_note(repaired$invalid, basename(path)))
  }
  data
}

# The separator fread chose, for the report: the most frequent candidate in the header line.
detect_delimiter <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  candidates <- c("," = ",", ";" = ";", "\t" = "\t", "|" = "|")
  # useBytes: a header in Windows-1252 ('Länge') must not stop the count
  counts <- vapply(candidates, function(d) lengths(regmatches(header, gregexpr(d, header, fixed = TRUE, useBytes = TRUE))), integer(1))
  names(candidates)[which.max(counts)]
}

# Excel: read every cell with its own type, then turn it into text.
read_excel_as_text <- function(path, sheet = NULL) {
  sheets <- readxl::excel_sheets(path)
  if (is.null(sheet)) sheet <- sheets[1]
  if (is.numeric(sheet)) sheet <- sheets[sheet]
  if (!sheet %in% sheets) stop("No sheet '", sheet, "'. Sheets: ", paste(sheets, collapse = ", "))

  cells <- readxl::read_excel(path, sheet = sheet, col_types = "list", .name_repair = "minimal")
  names(cells) <- make.unique(ifelse(is.na(names(cells)) | names(cells) == "", "unnamed", names(cells)), sep = "_")

  dates_converted <- 0L
  data <- as.data.table(lapply(cells, function(column) {
    is_date <- vapply(column, inherits, logical(1), what = "POSIXct")
    dates_converted <<- dates_converted + sum(is_date)
    vapply(column, cell_as_text, character(1))
  }))
  setattr(data, "sheet", sheet)
  if (dates_converted > 0) {
    setattr(data, "notes", sprintf("%d Excel date cells were written as ISO 8601 dates", dates_converted))
  }
  data
}

# One Excel cell as text.
cell_as_text <- function(value) {
  if (is.null(value) || length(value) == 0 || (length(value) == 1 && is.na(value))) return(NA_character_)
  if (inherits(value, "POSIXct")) {
    utc <- as.POSIXlt(value, tz = "UTC")
    has_time <- (utc$hour + utc$min + utc$sec) > 0
    return(format(value, if (has_time) "%Y-%m-%dT%H:%M:%S" else "%Y-%m-%d", tz = "UTC"))
  }
  if (is.numeric(value)) return(format(value, digits = 15, scientific = FALSE, trim = TRUE))
  if (is.logical(value)) return(tolower(as.character(value)))
  as.character(value)
}
