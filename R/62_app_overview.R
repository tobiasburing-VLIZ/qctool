# =============================================================================
# App screen 2: Overview. The verdict, the essentials (taxonomy, position,
# date, occurrenceStatus), the data checks with issues per table (filtered by
# severity: errors first, as they block the harvest), a short line about the
# metadata (details on screen 4), and the checks that could not run.
# =============================================================================

overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("top")),
    shiny::uiOutput(ns("per_table")),
    shiny::uiOutput(ns("bottom")))
}

overview_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$top <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(nothing_yet())
      counts <- severity_counts(r)
      top <- safely(shiny::tagList(
        verdict_card(r),
        could_not_run_section(r),   # right under the verdict: missing results must not go unnoticed
        bslib::layout_column_wrap(
          width = 1 / 4, class = "mt-3",
          bslib::value_box("Errors", format(counts[["error"]], big.mark = ","), "block the harvest: must be fixed",
                           theme = if (counts[["error"]] > 0) "danger" else "success"),
          bslib::value_box("Warnings", format(counts[["warning"]], big.mark = ","), "for the data managers to judge",
                           theme = if (counts[["warning"]] > 0) "warning" else "success"),
          bslib::value_box("Info", format(counts[["info"]], big.mark = ","), "hints and suggestions", theme = "light"),
          bslib::value_box("Records", format(sum(r$tables$records), big.mark = ","),
                           paste(sprintf("%s %s", format(r$tables$records, big.mark = ","), r$tables$table), collapse = " | "),
                           theme = "light")),
        essentials_section(r),
        links_section(r),
        one_to_one_section(r)))
      if (failed(top)) error_alert(top, "The overview") else top
    })

    # One card per data table; its check list is a separate output so that the
    # severity choice does not rebuild the page.
    output$per_table <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      tables <- data_tables_of(r)
      shiny::tagList(
        shiny::h4(class = "mt-4", "Data checks per table"),
        shiny::div(class = "d-flex align-items-center gap-3",
                   severity_input(ns("severity"), selected = shiny::isolate(if (is.null(input$severity)) "error" else input$severity), inline = TRUE)),
        lapply(tables, function(table_name) {
          bslib::card(class = "mt-2",
                      bslib::card_header(shiny::textOutput(ns(paste0("title_", table_name)), inline = TRUE)),
                      bslib::card_body(shiny::uiOutput(ns(paste0("checks_", table_name)))))
        }))
    })

    shiny::observeEvent(result(), {
      r <- result()
      titles <- titles_of_checks(r)
      for (table_name in data_tables_of(r)) local({
        this_table <- table_name
        records <- r$tables$records[match(this_table, r$tables$table)]
        output[[paste0("title_", this_table)]] <- shiny::renderText({
          counts <- issue_counts(data_problems(r)[table == this_table])
          sprintf("%s%s: %s errors, %s warnings, %s info", table_label(this_table),
                  if (is.na(records)) "" else sprintf(" (%s records)", format(records, big.mark = ",")),
                  format(sum(counts$errors), big.mark = ","), format(sum(counts$warnings), big.mark = ","),
                  format(sum(counts$info), big.mark = ","))
        })
        output[[paste0("checks_", this_table)]] <- shiny::renderUI({
          wanted <- input$severity   # not 'severity': inside problems[...] that would mean the column
          problems <- data_problems(r)[table == this_table & severity %chin% wanted]
          if (!nrow(problems)) {
            return(shiny::p(class = "text-muted mb-0", if (!length(wanted)) "Tick errors, warnings or info above."
                            else "Nothing to show for the ticked severities."))
          }
          summary <- safely(problem_summary(problems, titles))
          if (failed(summary)) return(error_alert(summary, "The problems of this table"))
          table <- if (nrow(summary) > 10) filter_table(summary, 10, selection = "none", dropdowns = c("severity", "problem", "field"))
                   else plain_table(summary, 10)
          shiny::tagList(colour_severity(table),
                         shiny::p(class = "small text-muted mt-2 mb-0", "The records with each problem: screen 3 (Data issues)."))
        })
      })
    })

    output$bottom <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      bottom <- safely(shiny::tagList(metadata_line(r), about_run_section(r)))
      if (failed(bottom)) error_alert(bottom, "Part of the overview") else bottom
    })
  })
}

# The verdict (verdict() in R/04_result.R) in the colour of its state.
verdict_card <- function(r) {
  judged <- verdict(r)
  colour <- c(errors = "danger", incomplete = "warning", warnings = "warning", clean = "success")[[judged$state]]
  title <- if (is.na(r$run$dataset_title)) r$run$source_name else r$run$dataset_title
  shiny::div(class = paste0("alert alert-", colour, " mt-3 mb-0"),
             shiny::h3(class = "alert-heading", judged$title),
             shiny::p(class = "mb-1", judged$text),
             shiny::p(class = "mb-0 small", shiny::strong(title), " | ", r$run$source_name, " | checked ", r$run$checked_at,
                      " in ", r$run$duration_s, " s", if (isFALSE(r$run$online)) " | offline run" else ""))
}

# The problems of one table in words a provider can act on: what is wrong, in
# which field, example values next to the proposed correction, how many
# records. One row per check, field and message. No record IDs: those are on
# screen 3, where the records are listed.
problem_summary <- function(problems, titles) {
  summary <- problems[, .(examples = value_examples(value, suggestion, records), records = sum(as.numeric(records))),
                      by = .(severity, check_id, field, message)]
  # Problems about whole columns (e.g. nine missing required columns) in one row, their fields listed.
  whole_columns <- summary[records == 0, .(field = paste(stats::na.omit(field), collapse = ", "), examples = examples[1], records = 0),
                           by = .(severity, check_id, message)]
  summary <- rbind(summary[records > 0], whole_columns, use.names = TRUE)
  summary <- summary[order(-severity_rank[severity], -records)]
  summary[, .(severity, problem = titles[check_id], field, `what is wrong` = message,
              `example values (and the correction)` = examples,
              records = ifelse(records == 0, "whole column or table", format(records, big.mark = ",")))]
}

# Up to n values (those with most records first), each with its suggested
# correction where the check has one: "'9811'", "'Ã©' -> 'é'".
value_examples <- function(values, suggestions, records, n = 3) {
  keep <- !is.na(values) & nzchar(values)
  if (!any(keep)) return("")
  by_records <- order(-records[keep])
  values <- values[keep][by_records]
  suggestions <- suggestions[keep][by_records]
  first <- !duplicated(values)
  values <- values[first]
  suggestions <- suggestions[first]
  shown <- seq_len(min(n, length(values)))
  quoted <- ifelse(grepl("^'", values[shown]), shorten(values[shown], 70), sprintf("'%s'", shorten(values[shown], 70)))
  text <- ifelse(is.na(suggestions[shown]) | !nzchar(suggestions[shown]), quoted,
                 sprintf("%s → %s", quoted, shorten(suggestions[shown], 90)))
  more <- length(values) - length(shown)
  paste0(paste(text, collapse = "; "), if (more > 0) sprintf(" (and %s more)", format(more, big.mark = ",")) else "")
}

essentials_section <- function(r) {
  essentials <- qc_essentials(r)
  themes <- c(green = "success", orange = "warning", red = "danger", grey = "secondary")
  boxes <- lapply(seq_len(nrow(essentials)), function(i) {
    e <- essentials[i]
    counted <- sprintf("%s of %s occurrences without errors%s", format(e$ok, big.mark = ","), format(e$occurrences, big.mark = ","),
                       if (e$with_warnings > 0) sprintf("; %s with warnings", format(e$with_warnings, big.mark = ",")) else "")
    detail <- if (!nzchar(e$note)) counted else if (nzchar(e$not_run)) paste0(counted, ". ", toupper(substr(e$note, 1, 1)), substring(e$note, 2)) else e$note
    bslib::value_box(e$essential, if (is.na(e$ok_percent)) "-" else paste0(e$ok_percent, " %"), detail,
                     shiny::span(class = "small", e$fields), theme = themes[[e$light]])
  })
  shiny::tagList(
    shiny::h4(class = "mt-4", "Essentials"),
    shiny::p(class = "text-muted", "Every EurOBIS record needs a taxon, a position, a date and an occurrenceStatus.",
             "The share of occurrences without errors in these fields (also when the value comes from the event).",
             "Grey: not fully checked, because some of its checks did not run."),
    do.call(bslib::layout_column_wrap, c(list(width = 1 / 4), boxes)))
}

# The links between the tables (IDs, parent events, core links): the most
# common real errors, so they get their own place, right under the essentials.
links_section <- function(r) {
  short_checks_section(
    qc_links(r), heading = "Links between tables",   # none for a single table: nothing is linked
    intro = c("Every occurrence, measurement and DNA record must point to an event or occurrence",
              "that exists, every parentEventID to an existing event, and every eventID and occurrenceID must be unique.",
              "Records with a broken link get lost at the harvest."),
    all_fine = "All links are intact.", records_column = "records with a broken link",
    where = "The records, with their links in the column 'id (core link)': screen 3 (Data issues), the table named here.")
}

# Values and their IDs: one scientificName with two scientificNameIDs, one
# measurementType with two measurementTypeIDs, ... Right under the links.
one_to_one_section <- function(r) {
  short_checks_section(
    qc_one_to_one(r), heading = "Values and their IDs",
    intro = c("A value and its ID go one-to-one: each scientificName has one scientificNameID, each measurementType one",
              "measurementTypeID, each unit one unitID, and the other way round. One value with two IDs means one ID is",
              "wrong; one ID with two values is usually a spelling difference."),
    all_fine = "Every value goes with one ID.", records_column = "records with a less common combination",
    where = "The records, with the combination most records use as suggestion: screen 3 (Data issues), the table named here.")
}

# A heading, a short text and the result of some checks: green when all are
# fine, otherwise a table of what was found (red for errors).
short_checks_section <- function(short, heading, intro, all_fine, records_column, where) {
  if (!nrow(short)) return(NULL)
  found <- short[records > 0]
  intro <- shiny::p(class = "text-muted", paste(intro, collapse = " "))
  fine <- unique(short[records == 0, problem])
  checked <- shiny::p(class = "small text-muted mb-0", sprintf("Checked and fine (%d): %s.", length(fine), paste(fine, collapse = "; ")))
  if (!nrow(found)) {
    return(shiny::tagList(shiny::h4(class = "mt-4", heading), intro,
                          alert(shiny::tagList(shiny::strong(all_fine), " ", checked), "success")))
  }
  shown <- found[, .(severity, problem, table = table_label(table), records = format(records, big.mark = ","))]
  setnames(shown, "records", records_column)
  shiny::tagList(
    shiny::h4(class = "mt-4", heading), intro,
    bslib::card(class = if ("error" %in% found$severity) "border-danger" else "border-warning", bslib::card_body(
      colour_severity(plain_table(shown, 15)),
      shiny::p(class = "small text-muted mt-2 mb-1", where),
      if (length(fine)) checked)))
}

# The metadata in one line; the details are on screen 4.
metadata_line <- function(r) {
  if (identical(r$dataset$source$type, "table")) {
    return(bslib::card(class = "mt-4", bslib::card_header("Metadata (EML)"),
                       bslib::card_body(shiny::p(class = "mb-0", "A simple table has no metadata. When the data is published as a",
                                                 "Darwin Core Archive (e.g. on an IPT), the metadata is checked too."))))
  }
  metadata <- metadata_problems(r)
  fair <- qc_fair(r)
  bslib::card(class = "mt-4",
              bslib::card_header("Metadata (EML)"),
              bslib::card_body(shiny::p(class = "mb-0",
                sprintf("%d errors, %d warnings, %d info. FAIR: %s. Details on screen 4 (Metadata).",
                        sum(metadata$severity == "error"), sum(metadata$severity == "warning"), sum(metadata$severity == "info"),
                        paste(fair$summary$text, collapse = " | ")))))
}

could_not_run_section <- function(r) {
  failed_checks <- r$checks[status %chin% not_run_statuses][order(-severity_rank[severity])]
  if (!nrow(failed_checks)) return(NULL)
  shown <- failed_checks[, .(severity, check = title, group = ifelse(group %in% names(group_names), group_names[group], group), status, reason)]
  bslib::card(class = "mt-3 border-warning",
              bslib::card_header(sprintf("%d %s did not run: their results are missing", nrow(shown), if (nrow(shown) == 1) "check" else "checks")),
              shiny::p(class = "text-muted mx-3 mt-2 mb-0", "Nothing is known about these checks, also not that they passed.",
                       "'Not performed' usually means a service (WoRMS, NERC, OBIS) could not be reached, or the check ran offline:",
                       "check again later. 'Skipped' means a check it needs did not run. 'Check error' is a fault in the tool: please report it."),
              bslib::card_body(colour_severity(plain_table(shown))))
}

about_run_section <- function(r) {
  status_counts <- r$checks[, .N, by = status][order(match(status, status_levels))]
  lookups <- if (is.null(r$lookups) || !nrow(r$lookups)) NULL else {
    shiny::p(shiny::strong("Online lookups: "),
             paste(sprintf("%s %s %s (%s from the stored answers)", lookup_labels[r$lookups$service],
                           format(r$lookups$from_cache + r$lookups$from_internet, big.mark = ","), r$lookups$what,
                           format(r$lookups$from_cache, big.mark = ",")), collapse = "; "))
  }
  bslib::accordion(open = FALSE, class = "mt-4 mb-4",
    bslib::accordion_panel("About this check",
      shiny::p(shiny::strong("Checks: "), paste(sprintf("%s %d", status_counts$status, status_counts$N), collapse = " | ")),
      lookups,
      if (length(r$run$reading_notes)) shiny::p(shiny::strong("Notes from reading the data: "), paste(r$run$reading_notes, collapse = "; ")),
      shiny::p(shiny::strong("Rules: "), r$run$rules_file),
      shiny::p(shiny::strong("Tool: "), r$run$tool, r$run$tool_version)))
}
