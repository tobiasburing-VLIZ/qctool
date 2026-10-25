# =============================================================================
# qctool - QC Tool (Beta)
#
# Quality control of Darwin Core datasets for EMODnet Biology / EurOBIS.
#
# Where to start reading:
#   01_qc_check.R            the main function qc_check()
#   02_engine.R              how the checks are planned and run
#   03_issues.R              how a check reports a problem (new_issues)
#   checks_NN_<group>.R      the checks themselves, one small function each
#   inst/rules/qc_rules.xlsx which checks run, how severe, which fields are required
#
# Naming rule: ONLY check functions start with "check_". The engine finds a
# check by its name: the row 'occurrenceid_unique' in qc_rules.xlsx runs the
# function check_occurrenceid_unique(). Helper functions use other names.
# =============================================================================

#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom utils adist head
NULL

# Column names used inside data.table expressions. Declaring them stops
# R CMD check from reporting them as undefined variables.
utils::globalVariables(c(
  ".", ".N", ".SD", ".I", ".GRP", ".line", ".coreid", "..columns",
  "check_id", "severity", "status", "table", "file", "line", "field", "value",
  "message", "suggestion", "record_id", "record_id_field", "core_id",
  "affected_records", "severity_hint", "depends_on", "requirement", "term",
  "term_iri", "applies_to", "checked_in", "records", "group", "title",
  "issues", "records_affected", "percent_affected", "reason", "duration_s",
  "added_as_dependency", "settings", "n", "example_records", "first_line",
  "problem", "row", "label", "check", "measurement_type",
  # used in the checks
  "start", "end", "west", "east", "north", "south", "role", "given_name", "sur_name", "organization",
  "other_ids", "orcid", "thesaurus", "type", "unit", "number", "usual", "flagged", "value_id", "n_ids",
  "some_without", "usual_id", "n_units", "different_values", "owner", "size", "first", "emof_1", "emof_2",
  "count", "species", "lat", "lon", "distance", "limit", "wanted_role", "values", "wanted", "applies_to",
  "event_link", "group", "dna_sequence", "column", "position",
  # online lookups (R/3x_lookup_*.R and the checks that use them)
  "query", "found", "fetched", "rank_id", "valid_id", "is_marine", "is_brackish", "is_freshwater", "is_terrestrial",
  "worms_name", "pair", "collection", "deprecated", "replaced_by", "dimension", "alt", "linked", "linked_labels",
  "linked_units", "type_label", "unit_found", "unit_label", "unit_kind", "same_kind", "same", "verdict", "text",
  "service", "what", "from_cache", "from_internet",
  # summaries and app
  "table_row", "fields_of_problem", "letter", "counted", "essential", "ok_percent", "ok", "occurrences",
  "with_warnings", "note", "fields", "errors", "warnings", "info", "where",
  # explore
  "period", "records", "taxon", "issues", "group", "nerc_label", "text_examples", "numbers", "median",
  "measurementType", "measurementTypeID", "measurementUnit", "measurementUnitID", "scientificName", "scientificNameID",
  "own_date", "own_position", "id", "level", "measurements", "with_occurrences", "label", "found",
  "mid", "count", "kind", "residual", "fitted", "far", "owner", "share_female", "female", "male", "other", "sex",
  "keep", "N", "track", "date", "x", "y", "section", "implemented", "internet", "what", "why", "notes", "group",
  "hover", "accepted_name", "rank", "status", "map_label"
))
