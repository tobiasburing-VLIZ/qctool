# =============================================================================
# Run qc_check() on every DwC-A and the two simple tables in the Datasets folder,
# and save the reports.
#
# Run from the qctool folder (RStudio: open qctool, then source this file), or:
#   Rscript dev/run_test_datasets.R            everything except FIFD
#   Rscript dev/run_test_datasets.R --all      including FIFD (8 million records)
#   Rscript dev/run_test_datasets.R --offline  no internet: only the lookup cache
#
# Reports go to ../output/<dataset name>/ : report.xlsx + CSV files.
# =============================================================================

devtools::load_all(quiet = TRUE)

datasets_folder <- normalizePath("../Datasets")
output_folder <- file.path(dirname(datasets_folder), "output")
include_big <- "--all" %in% commandArgs(trailingOnly = TRUE)
online <- !"--offline" %in% commandArgs(trailingOnly = TRUE)

archives <- list.files(datasets_folder, pattern = "\\.zip$", full.names = TRUE)
if (!include_big) archives <- archives[!grepl("fifd", archives)]
# Simple tables (not Darwin Core archives); the other CSV/XLSX files in the
# folder are notes and other material, not datasets.
tables <- file.path(datasets_folder, c("DataMunida.csv", "FULL-OYSTER-centroids-withREFs-OBIS-unified.xlsx"))
inputs <- c(archives, tables[file.exists(tables)])

overview <- list()
for (input in inputs) {
  name <- tools::file_path_sans_ext(basename(input))
  cat("\n==========", name, "\n")
  memory_before <- gc(reset = TRUE)

  time_check <- system.time(result <- qc_check(input, online = online))[["elapsed"]]
  print(result)

  folder <- file.path(output_folder, name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  time_export <- system.time({
    qc_export_xlsx(result, file.path(folder, "report.xlsx"))
    qc_export_csv(result, folder)
  })[["elapsed"]]

  peak_memory_gb <- sum(gc()[, ncol(gc())]) / 1024
  counts <- severity_counts(result)
  overview[[name]] <- data.table(
    dataset = name,
    records = sum(result$tables$records),
    errors = counts[["error"]],
    warnings = counts[["warning"]],
    info = counts[["info"]],
    problems = nrow(result$issues_grouped),
    checks_with_issues = sum(result$checks$status == "issues found"),
    checks_failed = sum(result$checks$status == "check error"),
    not_performed = sum(result$checks$status == "not performed"),
    seconds_check = round(time_check, 1),
    seconds_export = round(time_export, 1),
    peak_memory_gb = round(peak_memory_gb, 1))
}

cat("\n\n========== OVERVIEW\n")
overview <- rbindlist(overview)
print(overview)
fwrite(overview, file.path(output_folder, "overview.csv"))
