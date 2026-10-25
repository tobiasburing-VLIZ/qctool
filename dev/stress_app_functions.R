# =============================================================================
# Run every function the app uses on every dataset in ../Datasets, and report
# each failure with where it happened. Catches what makes the app crash.
#
#   Rscript dev/stress_app_functions.R            all datasets except FIFD
#   Rscript dev/stress_app_functions.R --offline  no internet (cache only)
# =============================================================================


devtools::load_all(quiet = TRUE)
options(width = 200)
online <- !"--offline" %in% commandArgs(trailingOnly = TRUE)

datasets <- normalizePath("../Datasets")
inputs <- c(list.files(datasets, pattern = "\\.zip$", full.names = TRUE),
            file.path(datasets, c("DataMunida.csv", "FULL-OYSTER-centroids-withREFs-OBIS-unified.xlsx")))
inputs <- inputs[!grepl("fifd", inputs) & file.exists(inputs)]

failures <- list()
attempt <- function(dataset, step, expr) {
  outcome <- tryCatch(withCallingHandlers(expr, error = function(e) {
    calls <- sys.calls()
    where <- vapply(calls, function(call) paste(deparse(call[[1]])[1], collapse = ""), character(1))
    attr(e, "where") <- paste(utils::tail(where[!grepl("^(tryCatch|withCallingHandlers|doTryCatch|attempt|eval|force)", where)], 6), collapse = " > ")
    stop(e)
  }), error = function(e) e)
  if (inherits(outcome, "error")) {
    failures[[length(failures) + 1]] <<- data.table(dataset = dataset, step = step, error = conditionMessage(outcome),
                                                    where = if (is.null(attr(outcome, "where"))) "" else attr(outcome, "where"))
    cat(sprintf("  FAILED %-28s %s\n", step, conditionMessage(outcome)))
    return(NULL)
  }
  outcome
}

for (input in inputs) {
  name <- basename(input)
  cat("\n=====", name, "\n")
  result <- attempt(name, "qc_check", qc_check(input, online = online))
  if (is.null(result)) next
  cat(sprintf("  checked: %s problems, %s checks could not run\n", nrow(result$issues_grouped),
              sum(result$checks$status %in% c("check error", "not performed", "skipped"))))
  errors <- result$checks[status == "check error", .(check_id, reason)]
  if (nrow(errors)) print(errors)
  attempt(name, "qc_essentials", qc_essentials(result))
  attempt(name, "qc_fair", qc_fair(result))
  attempt(name, "qc_map_points", qc_map_points(result))
  for (p in result$issues_grouped$problem) {
    records <- attempt(name, paste("qc_issues problem", p), qc_issues(result, problem = p, limit = 100))
    if (is.null(records)) next
    first <- records[!is.na(table_row)][1]
    if (nrow(first) && !is.na(first$table_row)) attempt(name, paste("qc_record problem", p), qc_record(result, first$table, first$table_row))
  }
  # Explore and map extras
  attempt(name, "qc_time_summary", qc_time_summary(result))
  attempt(name, "qc_taxa_summary", qc_taxa_summary(result))
  attempt(name, "qc_emof_summary", qc_emof_summary(result))
  attempt(name, "qc_event_levels", qc_event_levels(result))
  variables <- attempt(name, "qc_numeric_variables", qc_numeric_variables(result))
  if (!is.null(variables) && nrow(variables)) {
    for (v in variables$id) attempt(name, paste("qc_variable_values", v), qc_variable_values(result, v, variables))
    measured <- variables[kind == "emof"]
    if (nrow(measured) >= 2) {
      for (model in c("linear", "exponential", "power")) {
        attempt(name, paste("qc_relationship", model), qc_relationship(result, measured$id[1], measured$id[2], model, variables = variables))
      }
    }
  }
  attempt(name, "qc_sex_ratio", qc_sex_ratio(result))
  groupings <- attempt(name, "track_groupings", track_groupings(result))
  for (by in groupings) attempt(name, paste("qc_tracks", by), qc_tracks(result, by))
  attempt(name, "qc_export_xlsx", qc_export_xlsx(result, tempfile(fileext = ".xlsx")))
  attempt(name, "qc_export_csv", qc_export_csv(result, tempfile("csv_")))
  attempt(name, "qc_export_json", qc_export_json(result, tempfile(fileext = ".json")))
}

cat("\n===== FAILURES\n")
if (length(failures)) print(unique(rbindlist(failures), by = c("dataset", "error"))) else cat("none\n")
