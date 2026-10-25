# =============================================================================
# Write the check catalogue (the appendix of the manual) from the rules
# workbook: inst/manual/checks.md. Run it after changing the workbook or adding
# a check (manual, section 7.9):
#
#   Rscript dev/write_check_catalogue.R      (from the qctool folder)
# =============================================================================

devtools::load_all(quiet = TRUE)

catalogue <- qc_checks()
group_names <- c(structure = "Files and columns", identifiers = "Identifiers", required = "Required fields",
                 vocabulary = "Vocabularies", dates = "Dates", location = "Location", depth = "Depth",
                 taxonomy = "Taxonomy", emof = "Measurements (eMoF)", duplicates = "Duplicates", other = "Other fields",
                 dna = "DNA", outliers = "Outliers", metadata = "Metadata (EML)", versions = "Versions",
                 out_of_scope = "Out of scope")
clean <- function(x) gsub("\\|", "/", ifelse(is.na(x), "", gsub("\\s+", " ", x)))

lines <- c(
  "# Appendix: the check catalogue",
  "",
  sprintf("Made from the rules workbook on %s by `dev/write_check_catalogue.R`: %d checks, %d implemented.",
          format(Sys.Date()), nrow(catalogue), sum(catalogue$implemented == "yes")),
  "In the app: Help > Checks (with search). In R: `qc_checks(\"words\")`.",
  "")
groups <- intersect(names(group_names), unique(catalogue$group))
for (wanted_group in groups) {   # not "group": inside catalogue[...] that is the column
  part <- catalogue[catalogue$group == wanted_group]
  lines <- c(lines, sprintf("## %s", group_names[[wanted_group]]), "",
             "| Check | Severity | What | Online |", "|---|---|---|---|",
             sprintf("| **%s** (`%s`)%s | %s | %s | %s |", clean(part$title), part$check_id,
                     ifelse(part$implemented == "yes", "", " *not built yet*"), part$severity, clean(part$what),
                     ifelse(nzchar(part$internet), clean(part$internet), "")),
             "")
}

# Where each check lives: its code, its settings (in the rules workbook) and
# the lists it reads. Only the checks that are built.
built <- catalogue[implemented == "yes"]
code_file <- sub(", function.*$", "", built$code)
lines <- c(lines,
  "## Where each check lives",
  "",
  "For every check that is built: the file with its code (folder `R/` of the tool, function `check_<check_id>()`),",
  "its settings (column *settings* of the sheet *Checks* in the rules workbook; change the numbers there) and the",
  "lists it reads. Lists in `reference/` are the CSV files in `inst/reference/` (manual, section 7.2).",
  "")
for (wanted_group in groups) {
  part <- which(built$group == wanted_group)
  if (!length(part)) next
  lines <- c(lines, sprintf("### %s", group_names[[wanted_group]]), "",
             "| Check | Code | Settings | Values from |", "|---|---|---|---|",
             sprintf("| %s (`%s`) | `%s` | %s | %s |", clean(built$title[part]), built$check_id[part], code_file[part],
                     ifelse(is.na(built$settings[part]) | !nzchar(built$settings[part]), "", paste0("`", clean(built$settings[part]), "`")),
                     clean(built$reference_list[part])),
             "")
}
file <- file.path("inst", "manual", "checks.md")
writeLines(lines, file, useBytes = TRUE)
cat("Written:", file, "(", nrow(catalogue), "checks )\n")

# The file of each check, for Help > Checks in an installed package (where R no
# longer knows the source files).
fwrite(data.table(check_id = built$check_id, file = code_file), file.path("inst", "reference", "check_code_files.csv"))
cat("Written: inst/reference/check_code_files.csv\n")
