# =============================================================================
# What a dataset looks like inside the tool
#
# Every reader (DwC-A now, CSV/XLSX tables later) produces the same object, so
# the checks never need to know where the data came from:
#
#   dataset$tables    named list of data.tables: event, occurrence, emof, dna, ...
#                     - one column per Darwin Core term, named by the term
#                       (decimalLatitude, eventDate, ...), ALL VALUES AS TEXT
#                     - empty cells are NA; nothing else is changed (spaces,
#                       placeholders, capitals stay, so checks can see them)
#                     - .line   : line number in the original file
#                     - .coreid : the core id / coreid column (link to the core)
#   dataset$core      name of the core table ("event" or "occurrence")
#   dataset$files     one row per data file: table, file, row type, delimiter,
#                     header lines, header names (first line), records, ...
#   dataset$fields    the meta.xml mapping: table, column, term IRI, index, default
#   dataset$problems  problems met while reading (reported by archive_readable)
#   dataset$notes     how the data was read, when worth knowing (not problems),
#                     e.g. "1,666 Excel date cells were written as ISO 8601 dates"
#   dataset$invalid_text  per table: the values that were not valid UTF-8 in the
#                     file and were read as Windows-1252 (column, row); reported
#                     by text_encoding_valid. See repair_invalid_text() in R/40_helpers.R
#   dataset$mapping   tables only: which column became which term, and how
#   dataset$eml       the eml.xml document (xml2), or NULL
#   dataset$metadata  the EML read into plain R lists (R/13_read_eml.R)
#   dataset$info      title, IPT URL, ... (from the EML)
#   dataset$source    where the data came from: type, path, file name, checksum, URL
#   dataset$cache     links between tables and inherited values, computed once
#                     (R/20_prepare.R)
#   dataset$rules     during a check run: the rules workbook (for the field lists)
# =============================================================================

new_dataset <- function(path, type) {
  structure(list(
    source   = list(type = type, path = path, name = basename(path),
                    checksum = if (file.exists(path) && !dir.exists(path)) unname(tools::md5sum(path)) else NA_character_),
    core     = NA_character_,
    tables   = list(),
    files    = data.table(table = character(), file = character(), row_type = character(),
                          is_core = logical(), delimiter = character(), quote = character(),
                          header_lines = integer(), encoding = character(), id_index = integer(),
                          records = integer(), columns = integer(), header_names = list()),
    fields   = data.table(table = character(), column = character(), term_iri = character(),
                          index = integer(), default = character()),
    problems = data.table(file = character(), message = character()),
    notes    = character(),
    invalid_text = list(),
    mapping  = NULL,
    eml      = NULL,
    metadata = NULL,
    info     = list(title = NA_character_, ipt_url = NA_character_),
    cache    = new.env(parent = emptyenv())   # links and inherited values, see R/20_prepare.R
  ), class = "qc_dataset")
}

# Record a problem met while reading. Reading continues where possible;
# the check archive_readable reports all problems.
add_problem <- function(dataset, file, message) {
  dataset$problems <- rbind(dataset$problems, data.table(file = file, message = message))
  dataset
}

#' @export
print.qc_dataset <- function(x, ...) {
  cat("Dataset:", if (is.na(x$info$title)) x$source$name else x$info$title, "\n")
  cat("Source: ", x$source$path, "\n")
  for (name in names(x$tables)) {
    cat(sprintf("  %-12s %9s records  %3d columns%s\n", name,
                format(nrow(x$tables[[name]]), big.mark = ","), length(data_columns(x$tables[[name]])),
                if (identical(name, x$core)) "  (core)" else ""))
  }
  if (nrow(x$problems)) cat("Problems while reading:", nrow(x$problems), "\n")
  for (note in x$notes) cat("Note:", note, "\n")
  if (!is.null(x$mapping)) {
    cat("Column mapping:\n")
    shown <- x$mapping[, .(column, term = ifelse(is.na(term), "(not used)", term), how)]
    print(shown, row.names = FALSE)
  }
  invisible(x)
}

# The Darwin Core columns of a table, without the internal .line / .coreid.
data_columns <- function(table) {
  grep("^\\.", names(table), value = TRUE, invert = TRUE)
}

# The file a table was read from, e.g. "occurrence.txt".
table_file <- function(dataset, table_name) {
  file <- dataset$files$file[dataset$files$table == table_name]
  if (length(file)) file[1] else NA_character_
}

# Names of the tables that contain all the given fields.
tables_with_fields <- function(dataset, fields) {
  has_all <- vapply(dataset$tables, function(table) all(fields %in% names(table)), logical(1))
  names(dataset$tables)[has_all]
}
