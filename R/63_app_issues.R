# =============================================================================
# App screen 3: Data issues, per table (event, occurrence, eMoF, ...).
# Problems (one row per identical problem) -> the records with that problem,
# with all their values -> one record: all its issues (also those of the events
# above it), its values, those events and its measurements.
# Every table has a filter box above each column. The records of one problem
# are shown up to 10,000 (all of them: CSV download). The metadata (EML) has
# its own screen (66_app_metadata.R).
# =============================================================================

records_shown <- 10000

issues_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    class = "mt-3",
    sidebar = bslib::sidebar(
      width = 260,
      severity_input(ns("severity")),
      shiny::selectizeInput(ns("check"), "Check", choices = NULL, multiple = TRUE, options = list(placeholder = "all checks")),
      shiny::helpText("Errors block the harvest; warnings and info are for the data managers to judge."),
      shiny::helpText("Click a problem to see its records; click a record to see everything about it.")),
    shiny::uiOutput(ns("nothing")),
    shiny::uiOutput(ns("table_tabs")),
    bslib::card(full_screen = TRUE, fill = FALSE,   # natural height: no small scrolling boxes
                bslib::card_header(shiny::textOutput(ns("problems_title"), inline = TRUE)),
                DT::DTOutput(ns("problems"), fill = FALSE)),
    bslib::card(full_screen = TRUE, fill = FALSE,   # natural height: no small scrolling boxes
                bslib::card_header(shiny::textOutput(ns("records_title"), inline = TRUE)),
                shiny::uiOutput(ns("records_note")),
                DT::DTOutput(ns("records"), fill = FALSE)),
    bslib::card(full_screen = TRUE, fill = FALSE,   # natural height: no small scrolling boxes
                bslib::card_header(shiny::textOutput(ns("record_title"), inline = TRUE)),
                shiny::uiOutput(ns("record")))
  )
}

issues_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$nothing <- shiny::renderUI(if (is.null(result())) nothing_yet())

    # One tab per data table (a tab is only a selector; the cards below follow it).
    output$table_tabs <- shiny::renderUI({
      r <- shiny::req(result())
      tables <- data_tables_of(r)
      counts <- lapply(tables, function(table_name) issue_counts(data_problems(r)[table == table_name]))
      tabs <- lapply(seq_along(tables), function(i) {
        bslib::nav_panel(sprintf("%s: %s errors, %s warnings", table_label(tables[i]),
                                 format(sum(counts[[i]]$errors), big.mark = ","), format(sum(counts[[i]]$warnings), big.mark = ",")),
                         value = tables[i])
      })
      # Open the first table with errors (they block the harvest), else the first with warnings.
      errors <- vapply(counts, function(x) sum(x$errors), numeric(1))
      warnings <- vapply(counts, function(x) sum(x$warnings), numeric(1))
      first <- if (any(errors > 0)) which(errors > 0)[1] else if (any(warnings > 0)) which(warnings > 0)[1] else 1
      do.call(bslib::navset_pill, c(list(id = ns("table"), selected = tables[first]), tabs))
    })

    in_table <- shiny::reactive({
      r <- shiny::req(result())
      chosen <- input$table
      if (is.null(chosen) || !chosen %in% data_tables_of(r)) chosen <- data_tables_of(r)[1]
      chosen
    })

    shiny::observeEvent(list(result(), in_table()), {
      grouped <- data_problems(result())[table == in_table()]
      titles <- titles_of_checks(result())
      checks <- sort(unique(grouped$check_id))
      shiny::updateSelectizeInput(session, "check", choices = stats::setNames(checks, titles[checks]))
    })

    problems <- shiny::reactive({
      grouped <- data_problems(shiny::req(result()))
      keep <- grouped$table == in_table() & grouped$severity %in% input$severity
      if (length(input$check)) keep <- keep & grouped$check_id %in% input$check
      grouped[keep]
    })

    output$problems_title <- shiny::renderText({
      all_in_table <- data_problems(result())[table == in_table()]
      hidden <- all_in_table[!severity %in% input$severity]
      hidden_records <- sum(pmax(hidden$records, 1L))
      sprintf("%s: %s problems (records with the same problem and value form one row)%s", table_label(in_table()),
              format(nrow(problems()), big.mark = ","),
              if (nrow(hidden) > 0) sprintf(". Not shown: %s problems (%s records) of other severities; tick them on the left",
                                            format(nrow(hidden), big.mark = ","), format(hidden_records, big.mark = ",")) else "")
    })

    output$problems <- DT::renderDT({
      titles <- titles_of_checks(result())
      shown <- problems()[, .(severity, problem = titles[check_id], field, value = shorten(value, 80),
                              `what is wrong` = message, suggestion = shorten(suggestion, 80), records)]
      colour_severity(filter_table(shown, dropdowns = c("severity", "problem", "field")))
    })

    # The chosen problem. A selection can be left over from the list before the
    # severity or table changed: then nothing is chosen.
    chosen_problem <- shiny::reactive({
      selected <- input$problems_rows_selected
      shiny::req(length(selected) == 1, selected <= nrow(problems()))
      problems()[selected]
    })

    # The records with the chosen problem, with all their values.
    records <- shiny::reactive({
      p <- chosen_problem()
      safely({
        found <- qc_issues(result(), problem = p$problem, limit = records_shown)[!is.na(table_row)]
        records_with_values(result(), p$table, found, fields = strsplit(p$field, "; ", fixed = TRUE)[[1]])
      })
    })

    output$records_title <- shiny::renderText({
      selected <- input$problems_rows_selected
      if (length(selected) != 1 || selected > nrow(problems())) return("Records: choose a problem above")
      p <- chosen_problem()
      if (isTRUE(p$records == 0)) return("This problem is about a whole column, a file or the table, not about single records")
      sprintf("Records with this problem: %s%s", format(p$records, big.mark = ","),
              if (p$records > records_shown) sprintf(" (the first %s shown; all in the CSV download)", format(records_shown, big.mark = ",")) else "")
    })

    output$records_note <- shiny::renderUI({
      if (length(input$problems_rows_selected) != 1) return(NULL)
      if (failed(records())) return(error_alert(records(), "The records"))
      shiny::p(class = "text-muted small mx-3 mt-2 mb-0", "Each row is the record as it is in the data; the columns of this problem are highlighted.")
    })

    output$records <- DT::renderDT({
      shown <- records()
      shiny::req(!failed(shown), nrow(shown) > 0)
      fields <- intersect(strsplit(chosen_problem()$field, "; ", fixed = TRUE)[[1]], names(shown))
      table <- filter_table(shown[, !"table_row"], dropdowns = character())
      if (length(fields)) table <- DT::formatStyle(table, fields, backgroundColor = "#FDEBD0", fontWeight = "bold")
      table
    })

    chosen_record <- shiny::reactive({
      selected <- input$records_rows_selected
      shiny::req(length(selected) == 1, !failed(records()))
      records()[selected]
    })

    output$record_title <- shiny::renderText({
      if (length(input$records_rows_selected) != 1) return("Record: choose a record above")
      r <- chosen_record()
      sprintf("Record %s (%s, line %s)", r$record_id, table_label(chosen_problem()$table), r$line)
    })

    output$record <- shiny::renderUI({
      shiny::req(length(input$records_rows_selected) == 1)
      info <- safely(qc_record(result(), chosen_problem()$table, chosen_record()$table_row))
      if (failed(info)) return(error_alert(info, "This record"))
      shiny::tagList(
        if (!is.na(info$link_note)) alert(shiny::tagList(shiny::strong("Broken link: "), utf8_safe(info$link_note)), "danger"),
        bslib::navset_underline(
        bslib::nav_panel(sprintf("Issues (%d)", nrow(info$issues)), colour_severity(plain_table(info$issues))),
        bslib::nav_panel("Values", plain_table(info$values, 50)),
        bslib::nav_panel(sprintf("Events above it (%d)", uniqueN(info$parents$level)),
                         if (nrow(info$parents)) plain_table(info$parents, 50) else shiny::p(class = "text-muted mt-2", "No events above this record.")),
        bslib::nav_panel(sprintf("Measurements (%d)", nrow(info$measurements)),
                         if (nrow(info$measurements)) plain_table(info$measurements, 50) else shiny::p(class = "text-muted mt-2", "No measurements for this record."))))
    })
  })
}

# Records as they are in the data: record ID and line, the problem's columns
# first, then all other columns of the table. Keeps table_row (for qc_record()).
records_with_values <- function(result, table_name, found, fields = character()) {
  data <- result$dataset$tables[[table_name]]
  pointers <- found[, .(record_id, line, table_row)]
  if (is.null(data) || !nrow(found)) return(pointers)
  columns <- data_columns(data)
  columns <- c(intersect(fields, columns), setdiff(columns, fields))
  # The link to the core (the archive's id column) right after the record ID:
  # that is where a broken link shows.
  cbind(pointers, link_values(result$dataset, table_name, found$table_row), data[found$table_row, ..columns])
}
