# =============================================================================
# Test helper: build a small Darwin Core Archive folder from data frames
#
#   folder <- make_archive(
#     event      = data.frame(id = "E1", eventID = "E1", eventDate = "2020-05-01"),
#     occurrence = data.frame(id = "E1", occurrenceID = "O1", scientificName = "Abra alba")
#   )
#   dataset <- qc_read_dwca(folder)
#
# The first table is the core. The column 'id' becomes the id / coreid column;
# every other column is mapped to the term with the same name.
# =============================================================================

row_type_of <- c(
  event      = "http://rs.tdwg.org/dwc/terms/Event",
  occurrence = "http://rs.tdwg.org/dwc/terms/Occurrence",
  emof       = "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact",
  mof        = "http://rs.tdwg.org/dwc/terms/MeasurementOrFact",
  dna        = "http://rs.gbif.org/terms/1.0/DNADerivedData",
  taxon      = "http://rs.tdwg.org/dwc/terms/Taxon"
)

term_iri_of <- function(name) {
  terms <- term_table()
  iri <- terms$term_iri[match(name, terms$term)]
  ifelse(is.na(iri), paste0("http://rs.tdwg.org/dwc/terms/", name), iri)
}

make_archive <- function(..., header = TRUE, delimiter = "\t", quote = FALSE, eml_title = "Test dataset",
                         defaults = list(), with_eml = TRUE) {
  tables <- list(...)
  folder <- tempfile("test_archive_")
  dir.create(folder)
  separator_in_meta <- if (delimiter == "\t") "\\t" else delimiter
  quote_in_meta <- if (quote) "&quot;" else ""

  blocks <- character()
  for (i in seq_along(tables)) {
    name <- names(tables)[i]
    data <- tables[[i]]
    file <- paste0(name, ".txt")
    fwrite(data, file.path(folder, file), sep = delimiter, quote = quote, col.names = header, na = "")

    tag <- if (i == 1) "core" else "extension"
    id_tag <- if (i == 1) "id" else "coreid"
    columns <- names(data)
    field_lines <- sprintf('    <field index="%d" term="%s"/>', which(columns != "id") - 1, term_iri_of(columns[columns != "id"]))
    table_defaults <- defaults[[name]]
    if (length(table_defaults)) {
      field_lines <- c(field_lines, sprintf('    <field term="%s" default="%s"/>', term_iri_of(names(table_defaults)), table_defaults))
    }
    blocks <- c(blocks,
      sprintf('  <%s encoding="UTF-8" fieldsTerminatedBy="%s" linesTerminatedBy="\\n" fieldsEnclosedBy="%s" ignoreHeaderLines="%d" rowType="%s">',
              tag, separator_in_meta, quote_in_meta, as.integer(header), row_type_of[[name]]),
      sprintf("    <files><location>%s</location></files>", file),
      sprintf('    <%s index="%d"/>', id_tag, match("id", columns) - 1),
      field_lines,
      sprintf("  </%s>", tag))
  }
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<archive xmlns="http://rs.tdwg.org/text/" metadata="eml.xml">', blocks, "</archive>"),
             file.path(folder, "meta.xml"))
  if (with_eml) {
    writeLines(sprintf('<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset><title>%s</title></dataset></eml:eml>', eml_title),
               file.path(folder, "eml.xml"))
  }
  folder
}

# A small, clean event-core dataset that passes all structure checks.
clean_archive <- function() {
  make_archive(
    event = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), eventDate = c("2020-05-01", "2020-05-02"),
                       decimalLatitude = c("51.2", "51.3"), decimalLongitude = c("2.9", "3.0")),
    occurrence = data.frame(id = c("E1", "E2"), eventID = c("E1", "E2"), occurrenceID = c("O1", "O2"),
                            scientificName = c("Abra alba", "Crangon crangon"), occurrenceStatus = c("present", "present")),
    emof = data.frame(id = c("E1", "E2"), occurrenceID = c("O1", "O2"), measurementType = c("abundance", "abundance"),
                      measurementValue = c("3", "5"))
  )
}

# Run one check on a dataset and return its issues as a detailed table
# (one row per issue and record, as qc_issues() gives it).
run_check <- function(check_id, dataset, settings = list()) {
  issues <- get(paste0("check_", check_id))(dataset, settings)
  issues_as_table(dataset, issues$problems, issues$rows)
}
