# =============================================================================
# Checks, group "structure": can the archive be read, are the files and
# columns as expected, is the text clean?
#
#   archive_readable                   core_type_supported
#   occurrence_table_present           table_not_empty
#   emof_extension_used                field_names_known
#   field_in_expected_table            field_datatype_valid
#   field_length_within_eurobis_limit  file_headers_valid
#   text_encoding_valid                no_line_breaks_in_values
#   no_leading_trailing_spaces         no_placeholder_values
#   no_empty_columns                   exact_duplicate_rows
#
# Every check: check_<check_id>(dataset, settings) -> issues table.
# Severity, texts and settings are in inst/rules/qc_rules.xlsx.
# =============================================================================

# -----------------------------------------------------------------------------
# CHECK  archive_readable
# What:  the zip or folder opens, meta.xml is valid and every file it lists exists.
# Why:   without a readable archive no other check can run.
# How:   reports the problems collected while reading (R/10_read_dwca.R).
# -----------------------------------------------------------------------------
check_archive_readable <- function(dataset, settings) {
  problems <- dataset$problems
  if (!nrow(problems)) return(empty_issues())
  new_issues(dataset, "archive", file = problems$file, message = problems$message)
}

# -----------------------------------------------------------------------------
# CHECK  core_type_supported
# What:  the core table is an Event or Occurrence core.
# Why:   EMODnet Biology accepts only these two; a Taxon core is a checklist.
# -----------------------------------------------------------------------------
check_core_type_supported <- function(dataset, settings) {
  core <- dataset$files[is_core == TRUE]
  if (!nrow(core)) skip_check("no core table")
  if (core$table[1] %in% c("event", "occurrence")) return(empty_issues())
  new_issues(dataset, "archive", file = core$file[1], field = "rowType", value = core$row_type[1],
             message = "The core is not an Event or Occurrence core")
}

# -----------------------------------------------------------------------------
# CHECK  occurrence_table_present
# What:  the archive contains an Occurrence table (core or extension).
# Why:   species records live in the Occurrence table.
# -----------------------------------------------------------------------------
check_occurrence_table_present <- function(dataset, settings) {
  if ("occurrence" %in% names(dataset$tables)) return(empty_issues())
  new_issues(dataset, "archive", file = "meta.xml",
             message = "The archive has no Occurrence table, so it contains no species records")
}

# -----------------------------------------------------------------------------
# CHECK  table_not_empty
# What:  every table listed in the archive has at least one record.
# Why:   an empty table usually means an export or mapping went wrong.
# -----------------------------------------------------------------------------
check_table_not_empty <- function(dataset, settings) {
  empty <- dataset$files[records == 0]
  if (!nrow(empty)) return(empty_issues())
  new_issues(dataset, "archive", file = empty$file, field = empty$table, message = "This table has no records")
}

# -----------------------------------------------------------------------------
# CHECK  emof_extension_used
# What:  measurements use the ExtendedMeasurementOrFact extension (eMoF).
# Why:   only eMoF links measurements to events and has the vocabulary ID fields.
# -----------------------------------------------------------------------------
check_emof_extension_used <- function(dataset, settings) {
  old <- dataset$files[row_type == "http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]
  if (!nrow(old)) return(empty_issues())
  new_issues(dataset, "archive", file = old$file, field = "rowType", value = old$row_type,
             message = "Measurements use the old MeasurementOrFact extension",
             suggestion = "Use http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact")
}

# -----------------------------------------------------------------------------
# CHECK  field_names_known
# What:  every term in meta.xml is a known Darwin Core / OBIS / DNA / Dublin Core
#        term (reference/dwc_terms.csv). Near misses get a suggestion.
# Why:   unknown or misspelt terms are silently dropped by OBIS, GBIF and EMODnet.
# -----------------------------------------------------------------------------
check_field_names_known <- function(dataset, settings) {
  fields <- dataset$fields[!is.na(term_iri) & !is_known_term_iri(term_iri)]
  if (!nrow(fields)) return(empty_issues())

  terms <- term_table()
  official_iri <- terms$term_iri[match(fields$column, terms$term)]
  known_name <- !is.na(official_iri)
  message <- ifelse(known_name,
                    "Known term name, but written with an unusual IRI (namespace) in meta.xml",
                    "Not a known Darwin Core, OBIS or DNA term; it will not be harvested")
  suggestion <- ifelse(known_name, official_iri, suggest_term(fields$column))

  for_each_table_of(fields, function(table_name, rows) {
    new_issues(dataset, table_name, field = fields$column[rows], value = fields$term_iri[rows],
               message = message[rows], suggestion = suggestion[rows])
  })
}

# Closest known term for a wrong name: capitals, synonym list, or a small typo.
suggest_term <- function(names) {
  terms <- term_table()$term
  synonyms <- reference_table("term_synonyms.csv")
  vapply(names, function(name) {
    same_letters <- match(tolower(name), tolower(terms))
    if (!is.na(same_letters)) return(terms[same_letters])
    synonym <- match(tolower(name), tolower(synonyms$wrong_name))
    if (!is.na(synonym)) return(synonyms$term[synonym])
    distance <- utils::adist(tolower(name), tolower(terms))[1, ]
    if (min(distance) <= 2) return(terms[which.min(distance)])
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# -----------------------------------------------------------------------------
# CHECK  field_in_expected_table
# What:  each term sits in a table where EurOBIS expects it (columns event_core,
#        occurrence_core, emof_extension, dna_extension of reference/dwc_terms.csv).
#        In an event-core dataset, event and location terms belong in the event
#        table, not in the occurrence extension.
# Why:   information in an unexpected table may not be harvested or is shown at
#        the wrong level.
# How:   only columns with at least one value are reported.
# -----------------------------------------------------------------------------
check_field_in_expected_table <- function(dataset, settings) {
  terms <- term_table()
  allowed_column <- c(event = "event_core", occurrence = "occurrence_core",
                      emof = "emof_extension", mof = "emof_extension", dna = "dna_extension")
  issue_list <- list()

  for (table_name in intersect(names(dataset$tables), names(allowed_column))) {
    table <- dataset$tables[[table_name]]
    columns <- mapped_columns(table)
    columns <- columns[vapply(columns, function(column) any(!is.na(table[[column]])), logical(1))]
    info <- terms[match(columns, terms$term)]

    not_allowed <- !is.na(info$term) & info[[allowed_column[[table_name]]]] != "TRUE"
    event_level_in_occurrence <- identical(dataset$core, "event") & table_name == "occurrence" &
      info$class_group %in% c("Event", "Location") & columns != "eventID"
    misplaced <- not_allowed | event_level_in_occurrence
    if (!any(misplaced)) next

    expected_in <- apply(info[misplaced, unique(unname(allowed_column)), with = FALSE], 1, function(flags) {
      paste(unique(names(allowed_column)[match(names(flags)[flags == "TRUE"], allowed_column)]), collapse = ", ")
    })
    issue_list[[table_name]] <- new_issues(
      dataset, table_name, field = columns[misplaced],
      value = sprintf("filled in %s records", format(vapply(columns[misplaced], function(column) sum(!is.na(table[[column]])), integer(1)), big.mark = ",")),
      message = ifelse(event_level_in_occurrence[misplaced],
                       "Event or location information in the occurrence table; in an event-core dataset it belongs in the event table",
                       sprintf("EurOBIS does not expect this term in the %s table", table_name)),
      suggestion = ifelse(event_level_in_occurrence[misplaced], "move to the event table", paste("expected in:", expected_in)))
  }
  combine_issues(issue_list)
}

# -----------------------------------------------------------------------------
# CHECK  field_datatype_valid
# What:  values fit the data type of their term (reference/dwc_terms.csv):
#        decimal, integer, ISO 8601 date or time, URI, boolean.
# Why:   values of the wrong type cannot be stored; the EurOBIS harvester rejects them.
# How:   fields with their own dedicated check are skipped (no double reports);
#        placeholders (NA, NULL, ...) and surrounding spaces are left to their own
#        checks. Tests each different value once.
# -----------------------------------------------------------------------------
check_field_datatype_valid <- function(dataset, settings) {
  types <- term_table()[, .(term, data_type)]
  for_each_column(dataset, skip_fields = fields_with_dedicated_checks, function(table_name, column, values) {
    type <- types$data_type[match(column, types$term)]
    if (is.na(type) || !type %in% names(datatype_tests)) return(empty_issues())
    test <- datatype_tests[[type]]

    rows <- rows_where(values, function(v) {
      clean <- trimws(v)
      !is_placeholder(v) & validUTF8(v) & !test(clean)
    })
    if (!length(rows)) return(empty_issues())
    new_issues(dataset, table_name, rows, field = column, value = values[rows],
               message = datatype_messages[[type]], suggestion = datatype_suggestion(trimws(values[rows]), type))
  })
}

# Fields checked by their own, more specific check (with better hints).
fields_with_dedicated_checks <- c(
  "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
  "minimumDepthInMeters", "maximumDepthInMeters", "minimumElevationInMeters", "maximumElevationInMeters",
  "eventDate", "eventTime", "year", "month", "day", "startDayOfYear", "endDayOfYear", "dateIdentified",
  "individualCount", "DNA_sequence", "dynamicProperties", "footprintWKT",
  "license"   # a short name such as 'CC-BY' is fine (license_field_valid), although the term is a URI
)

datatype_tests <- list(
  "decimal"              = is_decimal_number,
  "integer"              = is_whole_number,
  "date/time (ISO 8601)" = is_iso8601,
  "time (ISO 8601)"      = is_iso8601_time,
  "URI"                  = is_uri,
  "boolean"              = function(v) tolower(v) %in% c("true", "false", "1", "0")
)

datatype_messages <- list(
  "decimal"              = "Not a number: expected a decimal number with a dot as decimal separator",
  "integer"              = "Not a whole number",
  "date/time (ISO 8601)" = "Not a valid ISO 8601 date (e.g. 2019-05-21 or 2019-05-21T14:30Z)",
  "time (ISO 8601)"      = "Not a valid ISO 8601 time (e.g. 14:30 or 14:30:00Z)",
  "URI"                  = "Not a valid URI (it should start with e.g. http://, https:// or urn:)",
  "boolean"              = "Not a valid true/false value"
)

# A fix when it is obvious: "1,5" -> "1.5", "2.0" -> "2".
datatype_suggestion <- function(values, type) {
  suggestion <- rep(NA_character_, length(values))
  if (type == "decimal") {
    with_dot <- sub(",", ".", values, fixed = TRUE)
    fixable <- grepl(",", values, fixed = TRUE) & is_decimal_number(with_dot)
    suggestion[fixable] <- with_dot[fixable]
  }
  if (type == "integer") {
    fixable <- grepl("^[+-]?\\d+\\.0+$", values)
    suggestion[fixable] <- sub("\\.0+$", "", values[fixable])
  }
  suggestion
}

# -----------------------------------------------------------------------------
# CHECK  field_length_within_eurobis_limit
# What:  no value is longer than the EurOBIS database column (error), or than a
#        lower limit of the current harvester (warning). Both limits are in the
#        sheet 'Field lengths' of the rules workbook (max_length, warning_above).
# Why:   longer values make the harvest fail.
# -----------------------------------------------------------------------------
check_field_length_within_eurobis_limit <- function(dataset, settings) {
  limits <- field_length_limits(dataset)
  database_limit <- limits$max_length
  harvester_limit <- limits$warning_above

  for_each_column(dataset, function(table_name, column, values) {
    i <- match(column, limits$term)
    if (is.na(i)) return(empty_issues())
    lowest <- suppressWarnings(min(database_limit[i], harvester_limit[i], na.rm = TRUE))
    if (!is.finite(lowest)) return(empty_issues())

    rows <- rows_where(values, function(v) nchar(v, type = "chars", allowNA = TRUE) > lowest)
    if (!length(rows)) return(empty_issues())
    lengths <- nchar(values[rows], type = "chars", allowNA = TRUE)
    too_long_for_database <- !is.na(database_limit[i]) & lengths > database_limit[i]
    new_issues(dataset, table_name, rows, field = column,
               value = sprintf("%s (%d characters)", shorten(values[rows], 60), lengths),
               message = ifelse(too_long_for_database,
                                sprintf("Longer than the EurOBIS database allows (%d characters)", database_limit[i]),
                                sprintf("Longer than the EurOBIS harvester currently accepts (%d characters)", harvester_limit[i])),
               severity_hint = ifelse(too_long_for_database, NA_character_, "warning"))
  })
}

# Length limits per term: the sheet 'Field lengths' of the rules workbook, or,
# for an own workbook without that sheet, reference/dwc_terms.csv.
field_length_limits <- function(dataset) {
  if (!is.null(dataset$rules$field_lengths)) return(dataset$rules$field_lengths)
  terms <- term_table()
  data.table(term = terms$term, max_length = database_length_limit(terms$sql_server_type),
             warning_above = suppressWarnings(as.integer(terms$harvester_limit)))
}

# -----------------------------------------------------------------------------
# CHECK  file_headers_valid
# What:  (a) data files without a header row; (b) header names that differ from
#        the term in meta.xml; (c) columns that meta.xml does not map to a term.
# Why:   without usable column names, archives from GBIF IPTs need a manual step.
# How:   compares ONLY the first line of each file with meta.xml; never reads
#        the data rows, so it costs the same for 100 or 10 million records.
#        Simple tables (CSV/Excel) have no meta.xml: there only (c) is reported,
#        for the columns the mapping did not give a term.
# -----------------------------------------------------------------------------
check_file_headers_valid <- function(dataset, settings) {
  if (identical(dataset$source$type, "table")) return(unmapped_table_columns(dataset))

  issue_list <- list()
  for (i in seq_len(nrow(dataset$files))) {
    file_info <- dataset$files[i]
    if (file_info$header_lines == 0) {
      issue_list[[length(issue_list) + 1]] <- new_issues(
        dataset, file_info$table, file = file_info$file,
        message = "The file has no header row: the column names exist only in meta.xml (the tool rebuilt them)",
        suggestion = "export the file with a header row of Darwin Core term names")
      next
    }

    header <- file_info$header_names[[1]]
    fields <- dataset$fields[table == file_info$table & !is.na(index)]
    in_header <- header[fields$index + 1]
    differs <- is.na(in_header) | in_header != fields$column
    if (any(differs)) {
      only_capitals <- !is.na(in_header) & tolower(in_header) == tolower(fields$column)
      issue_list[[length(issue_list) + 1]] <- new_issues(
        dataset, file_info$table, file = file_info$file, field = fields$column[differs],
        value = ifelse(is.na(in_header[differs]), "(no name)", in_header[differs]),
        message = ifelse(only_capitals[differs],
                         "The column name in the header differs only in capitals from the Darwin Core term in meta.xml",
                         "The column name in the header differs from the Darwin Core term in meta.xml"),
        suggestion = fields$column[differs])
    }

    unmapped <- setdiff(seq_along(header) - 1L, c(fields$index, file_info$id_index))
    if (length(unmapped)) {
      issue_list[[length(issue_list) + 1]] <- new_issues(
        dataset, file_info$table, file = file_info$file, field = sprintf("column %d", unmapped + 1L),
        value = header[unmapped + 1L],
        message = "This column is not mapped to any term in meta.xml and will not be harvested")
    }
  }
  combine_issues(issue_list)
}

# Simple tables: columns without a Darwin Core term. Mostly numbers -> could be
# an eMoF measurement (e.g. Mesh_size, Filter_volume in the Munida table).
unmapped_table_columns <- function(dataset) {
  unmapped <- dataset$mapping[is.na(term)]
  if (!nrow(unmapped)) return(empty_issues())
  data <- dataset$tables[[dataset$core]]
  # By position, not by name: a table can repeat a column name ('Group' twice).
  positions <- if ("position" %in% names(unmapped)) unmapped$position else match(unmapped$column, dataset$files$header_names[[1]])
  mostly_numbers <- vapply(positions, function(p) {
    values <- data[[paste0("column_", p)]]
    values <- utf8_safe(values[!is.na(values)])    # text in another encoding (e.g. '±' from Excel) must not stop the check
    length(values) > 0 && mean(is_decimal_number(trimws(values))) > 0.9
  }, logical(1))
  new_issues(dataset, dataset$core, field = sprintf("column %d", positions), value = unmapped$column,
             message = "This column is not mapped to a Darwin Core term and is not used",
             suggestion = ifelse(mostly_numbers, "map it to a term, or provide it as an eMoF measurement (it holds numbers)",
                                 "map it to a term if it holds Darwin Core information"))
}

# -----------------------------------------------------------------------------
# CHECK  text_encoding_valid
# What:  no invalid bytes, no replacement character, no broken accents
#        ('Ã©' instead of 'é'), no HTML codes (&quot;, &amp;).
# Why:   broken characters make names, places and people unreadable.
# How:   only values with non-ASCII characters or '&' are looked at closely.
#        Values that were not valid UTF-8 in the file were already read as
#        Windows-1252 by the reader (dataset$invalid_text, R/40_helpers.R):
#        they are reported here as they were read.
# -----------------------------------------------------------------------------
check_text_encoding_valid <- function(dataset, settings) {
  combine_issues(list(
    repaired_text_issues(dataset),
    for_each_column(dataset, function(table_name, column, values) {
      found <- find_problem_values(values, encoding_problem)
      if (!nrow(found)) return(empty_issues())
      new_issues(dataset, table_name, found$row, field = column, value = shorten(values[found$row]),
                 message = found$message, suggestion = repair_encoding(values[found$row]))
    })))
}

# The values the reader repaired (not valid UTF-8 in the file), per table.
repaired_text_issues <- function(dataset) {
  combine_issues(lapply(names(dataset$invalid_text), function(table_name) {
    invalid <- dataset$invalid_text[[table_name]]
    data <- dataset$tables[[table_name]]
    if (is.null(data) || !nrow(invalid)) return(NULL)
    invalid <- invalid[column %chin% names(data)]
    shown <- character(nrow(invalid))
    for (name in unique(invalid$column)) {
      in_column <- which(invalid$column == name)
      shown[in_column] <- data[[name]][invalid$row[in_column]]
    }
    new_issues(dataset, table_name, invalid$row, field = invalid$column, value = shorten(shown),
               message = "Not valid UTF-8 in the file (saved in another encoding, e.g. Windows-1252 by Excel); read here as Windows-1252",
               suggestion = "save the file as UTF-8")
  }))
}

encoding_problem <- function(values) {
  problem <- rep(NA_character_, length(values))
  candidate <- which(grepl("[^\\x01-\\x7F]|&", values, perl = TRUE, useBytes = TRUE))
  if (!length(candidate)) return(problem)

  v <- values[candidate]
  kind <- rep(NA_character_, length(v))
  valid <- validUTF8(v)
  kind[!valid] <- "Contains bytes that are not valid UTF-8 (the file was saved in another encoding)"
  kind[valid & grepl("�", v, fixed = TRUE)] <-
    "Contains the replacement character '�': characters were lost in an earlier conversion"
  todo <- valid & is.na(kind)
  kind[todo][grepl("Ã[\u0080-¿]|Â[\u0080-¿]|â€", v[todo], perl = TRUE)] <-
    "Contains broken accents (e.g. 'Ã©' instead of 'é'): UTF-8 text was read as Latin-1 somewhere"
  todo <- valid & is.na(kind)
  kind[todo][grepl("&(quot|amp|lt|gt|apos|nbsp|#\\d+|#x[0-9a-fA-F]+);", v[todo], perl = TRUE)] <-
    "Contains HTML codes (e.g. &quot; or &amp;) instead of the characters themselves"

  problem[candidate] <- kind
  problem
}

# Proposed repair: decode HTML codes, or undo a UTF-8-read-as-Latin-1 mix-up.
repair_encoding <- function(values) {
  repaired <- rep(NA_character_, length(values))
  valid <- validUTF8(values)
  html <- valid & grepl("&(quot|amp|lt|gt|apos|#39);", values, perl = TRUE)
  decoded <- values[html]
  for (code in names(html_codes)) decoded <- gsub(code, html_codes[[code]], decoded, fixed = TRUE)
  repaired[html] <- decoded

  broken <- valid & !html & grepl("Ã[\u0080-¿]", values, perl = TRUE)
  attempt <- iconv(values[broken], from = "UTF-8", to = "latin1")
  Encoding(attempt) <- "UTF-8"
  ok <- !is.na(attempt) & validUTF8(attempt)
  repaired[which(broken)[ok]] <- attempt[ok]
  shorten(repaired)
}

html_codes <- c("&quot;" = "\"", "&apos;" = "'", "&#39;" = "'", "&lt;" = "<", "&gt;" = ">", "&amp;" = "&")

# -----------------------------------------------------------------------------
# CHECK  no_line_breaks_in_values
# What:  values contain no tab characters or line breaks.
# Why:   they break tab-separated files and shift the columns.
# -----------------------------------------------------------------------------
check_no_line_breaks_in_values <- function(dataset, settings) {
  for_each_column(dataset, function(table_name, column, values) {
    rows <- rows_where(values, function(v) grepl("[\t\r\n]", v, useBytes = TRUE))
    if (!length(rows)) return(empty_issues())
    new_issues(dataset, table_name, rows, field = column,
               value = shorten(gsub("\r?\n|\r", " ↵ ", gsub("\t", " ⇥ ", values[rows]))),
               message = "Contains a line break or tab inside the value",
               suggestion = shorten(gsub("[\t\r\n]+", " ", values[rows])))
  })
}

# -----------------------------------------------------------------------------
# CHECK  no_leading_trailing_spaces
# What:  values have no spaces (also no-break spaces) at the start or end.
# Why:   'Abra alba ' and 'Abra alba' count as different values; IDs with
#        spaces fail to link.
# -----------------------------------------------------------------------------
check_no_leading_trailing_spaces <- function(dataset, settings) {
  for_each_column(dataset, function(table_name, column, values) {
    rows <- rows_where(values, function(v) text_matches(v, "^[\\s ]|[\\s ]$"))
    if (!length(rows)) return(empty_issues())
    new_issues(dataset, table_name, rows, field = column,
               value = sprintf("'%s'", shorten(values[rows])),
               message = "Space at the start or end of the value",
               suggestion = shorten(trimws(values[rows], whitespace = "[\\s ]")))
  })
}

# -----------------------------------------------------------------------------
# CHECK  no_placeholder_values
# What:  values are not placeholders such as NA, NULL, N/A, -, none or ?
#        (list: inst/reference/placeholders.csv).
# Why:   software reads placeholders as real values; leave the cell empty.
# -----------------------------------------------------------------------------
check_no_placeholder_values <- function(dataset, settings) {
  for_each_column(dataset, function(table_name, column, values) {
    rows <- rows_where(values, is_placeholder)
    if (!length(rows)) return(empty_issues())
    new_issues(dataset, table_name, rows, field = column, value = values[rows],
               message = "Placeholder text instead of an empty cell", suggestion = "leave the cell empty")
  })
}

is_placeholder <- function(values) per_value(values, placeholder_test)

placeholder_test <- function(values) {
  placeholders <- tolower(reference_table("placeholders.csv")$value)
  result <- logical(length(values))
  # Placeholders are short: only look closely at short values (fast for long texts and IDs).
  short <- which(nchar(values, type = "bytes") <= max(nchar(placeholders, type = "bytes")) + 4)
  short <- short[validUTF8(values[short])]
  result[short] <- tolower(trimws(values[short])) %in% placeholders
  result
}

# -----------------------------------------------------------------------------
# CHECK  no_empty_columns
# What:  a column is in the file but has no value in any record.
# Why:   usually a mapping leftover; for a required field it hides a missing value.
# -----------------------------------------------------------------------------
check_no_empty_columns <- function(dataset, settings) {
  issue_list <- list()
  for (table_name in names(dataset$tables)) {
    data <- dataset$tables[[table_name]]
    if (!nrow(data)) next
    in_file <- dataset$fields$column[dataset$fields$table == table_name & !is.na(dataset$fields$index)]
    columns <- intersect(mapped_columns(data), in_file)
    empty <- columns[vapply(columns, function(column) all(is.na(data[[column]])), logical(1))]
    if (length(empty)) {
      issue_list[[table_name]] <- new_issues(
        dataset, table_name, field = empty,
        message = sprintf("The column is in the file but empty in all %s records", format(nrow(data), big.mark = ",")))
    }
  }
  combine_issues(issue_list)
}

# -----------------------------------------------------------------------------
# CHECK  exact_duplicate_rows
# What:  rows identical in every column (including the link to the core) to an
#        earlier row of the same table.
# Why:   exact copies are almost always export errors and inflate counts.
# How:   the second, third, ... copy is reported; 'value' gives the line of the first.
# -----------------------------------------------------------------------------
check_exact_duplicate_rows <- function(dataset, settings) {
  issue_list <- list()
  for (table_name in names(dataset$tables)) {
    table <- dataset$tables[[table_name]]
    columns <- setdiff(names(table), ".line")
    if (nrow(table) < 2 || !length(columns)) next
    copies <- which(duplicated(table, by = columns))
    if (!length(copies)) next

    first_copies <- unique(table[copies, ..columns])
    first_copies[, first_line := table[first_copies, on = columns, mult = "first", .line]]
    first_line_of_copy <- first_copies[table[copies, ..columns], on = columns, first_line]
    issue_list[[table_name]] <- new_issues(
      dataset, table_name, copies, value = paste("same as line", first_line_of_copy),
      message = "This row is identical to an earlier row in every column",
      suggestion = "remove the copy")
  }
  combine_issues(issue_list)
}
