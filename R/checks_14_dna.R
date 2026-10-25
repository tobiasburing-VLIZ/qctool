# =============================================================================
# Checks, group "dna": DNA-derived data
#
#   dna_extension_present    dna_fields_complete    dna_sequence_valid
#
# Which fields a DNA-derived dataset needs: rows of the sheet 'Fields' with
# applies_to = 'dna-derived' (Manual V5 table 1).
# =============================================================================

# Signs that records are DNA-derived, other than the DNA extension itself.
dna_signals <- function(dataset) {
  occurrences <- dataset$tables$occurrence
  if (is.null(occurrences)) return(character())
  signals <- character()
  if (any(grepl("(?i)dna sequence reads", column_or_na(occurrences, "organismQuantityType"), perl = TRUE))) {
    signals <- c(signals, "organismQuantityType 'DNA sequence reads'")
  }
  if (any(!is_empty_value(column_or_na(occurrences, "associatedSequences")))) signals <- c(signals, "associatedSequences")
  if (any(grepl("(?i)ebi\\.ac\\.uk|ncbi\\.nlm\\.nih\\.gov|/ena/", column_or_na(occurrences, "materialSampleID"), perl = TRUE))) {
    signals <- c(signals, "ENA/MGnify/NCBI sample links in materialSampleID")
  }
  event_sample <- dataset$tables$event$materialSampleID
  if (any(grepl("(?i)ebi\\.ac\\.uk|ncbi\\.nlm\\.nih\\.gov", event_sample, perl = TRUE))) {
    signals <- c(signals, "ENA/MGnify/NCBI sample links in materialSampleID")
  }
  unique(signals)
}

is_dna_dataset <- function(dataset) {
  "dna" %in% names(dataset$tables) || length(dna_signals(dataset)) > 0
}

# -----------------------------------------------------------------------------
# CHECK  dna_extension_present
# What:  when records look DNA-derived, the archive has a DNA-derived data extension.
# Why:   target_gene, primers and sequences can only be given in that extension.
# -----------------------------------------------------------------------------
check_dna_extension_present <- function(dataset, settings) {
  signals <- dna_signals(dataset)
  if (!length(signals)) skip_check("no sign of DNA-derived data")
  if ("dna" %in% names(dataset$tables)) return(empty_issues())
  new_issues(dataset, "archive", file = "meta.xml", field = "DNA derived data extension",
             value = paste(signals, collapse = "; "),
             message = "The records look DNA-derived, but the archive has no DNA-derived data extension",
             suggestion = "add the extension http://rs.gbif.org/terms/1.0/DNADerivedData (target_gene, primers, DNA_sequence, ...)")
}

# -----------------------------------------------------------------------------
# CHECK  dna_fields_complete
# What:  the DNA fields of Manual V5 table 1 are present and filled: 'required'
#        rows give errors, 'recommended' rows warnings.
# -----------------------------------------------------------------------------
check_dna_fields_complete <- function(dataset, settings) {
  if (!is_dna_dataset(dataset)) skip_check("not a DNA-derived dataset")
  fields <- field_requirements(dataset)
  fields <- fields[fields$applies_to == "dna-derived" & fields$requirement %in% c("required", "recommended")]
  # Without the DNA extension its fields are covered by dna_extension_present.
  if (!"dna" %in% names(dataset$tables)) fields <- fields[fields$checked_in != "dna"]

  combine_issues(lapply(c("required", "recommended"), function(level) {
    selected <- fields[requirement == level]     # 'requirement' is the column
    label <- if (level == "required") "Required DNA field" else "Recommended DNA field"
    issues <- combine_issues(list(missing_columns(dataset, selected, paste(label, "is missing")),
                                  empty_values(dataset, selected, label)))
    if (level == "recommended" && nrow(issues$problems)) issues$problems$severity_hint <- "warning"
    issues
  }))
}

# -----------------------------------------------------------------------------
# CHECK  dna_sequence_valid
# What:  DNA_sequence holds only IUPAC nucleotide codes (ACGTURYSWKMBDHVN).
# -----------------------------------------------------------------------------
check_dna_sequence_valid <- function(dataset, settings) {
  dna <- require_fields(dataset, "dna", "DNA_sequence")
  values <- dna$DNA_sequence
  rows <- rows_where(values, function(v) !grepl("^[ACGTURYSWKMBDHVN]+$", v, ignore.case = TRUE) & !is_placeholder(v))
  cleaned <- toupper(gsub("[\\s-]", "", values[rows], perl = TRUE))
  new_issues(dataset, "dna", rows, field = "DNA_sequence", value = shorten(values[rows], 60),
             message = "DNA_sequence contains other characters than IUPAC nucleotide codes",
             suggestion = ifelse(grepl("^[ACGTURYSWKMBDHVN]+$", cleaned), shorten(cleaned, 60), NA_character_))
}
