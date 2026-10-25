# =============================================================================
# Explore > Tables: the data itself. Which tables there are and how their
# records are linked; the rows of a table (the first 100, or those a search
# finds in the whole table); its columns, how often each is filled.
# The functions behind it are in 49_tables.R (also usable in R).
# =============================================================================

rows_shown_first <- 100

tables_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "text-muted mt-2", "The data as it was read: the tables, how their records are linked, and their rows.",
             "Search a whole table below; only the rows found are shown."),
    shiny::uiOutput(ns("structure")),
    shiny::h5(class = "mt-4", "Rows"),
    bslib::layout_columns(
      col_widths = c(3, 3, 2, 3, 1), class = "align-items-end",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("column"), "Column", choices = NULL),
      shiny::selectInput(ns("how"), "Find rows where it", choices = c("is" = "is", "contains" = "contains", "is empty" = "empty",
                                                                     "(line number of the file)" = "line")),
      shiny::textInput(ns("value"), "Value", placeholder = "e.g. an occurrenceID"),
      shiny::div(class = "d-flex gap-1 mb-3",
                 shiny::actionButton(ns("find"), "Find", class = "btn-primary"),
                 shiny::actionButton(ns("reset"), "First rows", class = "btn-outline-secondary btn-sm"))),
    shiny::uiOutput(ns("rows_note")),
    DT::DTOutput(ns("rows"), fill = FALSE),
    shiny::h5(class = "mt-4", "Columns of this table"),
    DT::DTOutput(ns("columns"), fill = FALSE))
}

tables_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$structure <- shiny::renderUI({
      r <- shiny::req(result())
      structure <- safely(qc_tables(r))
      if (failed(structure)) return(error_alert(structure, "The list of tables"))
      plain_table(structure, 10)
    })

    shiny::observeEvent(result(), {
      tables <- result()$tables$table
      shiny::updateSelectInput(session, "table", choices = stats::setNames(tables, table_label(tables)))
    })

    shiny::observeEvent(input$table, {
      data <- result()$dataset$tables[[input$table]]
      if (!is.null(data)) shiny::updateSelectInput(session, "column", choices = utf8_safe(data_columns(data)))
    })

    # NULL: show the first rows; otherwise the outcome of the last search.
    searched <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(result(), input$table, input$reset), searched(NULL))
    shiny::observeEvent(input$find, {
      searched(safely(qc_find_rows(result(), input$table, input$column, input$value, how = input$how)))
    })

    found <- shiny::reactive({
      shiny::req(result(), input$table)
      outcome <- searched()
      if (is.null(outcome)) safely(qc_find_rows(result(), input$table, how = "first", limit = rows_shown_first)) else outcome
    })

    output$rows_note <- shiny::renderUI({
      f <- found()
      if (failed(f)) return(error_alert(f, "The search"))
      searching <- !is.null(searched())
      text <- if (!searching) {
        sprintf("The first %s of %s rows. 'line' is the line in the file.", format(nrow(f$rows), big.mark = ","), format(f$total, big.mark = ","))
      } else if (f$total == 0) {
        "No rows found."
      } else {
        sprintf("%s rows found%s.", format(f$total, big.mark = ","),
                if (f$total > nrow(f$rows)) sprintf(" (the first %s shown)", format(nrow(f$rows), big.mark = ",")) else "")
      }
      shiny::p(class = "text-muted small mb-1", text)
    })

    output$rows <- DT::renderDT({
      f <- found()
      shiny::req(!failed(f), nrow(f$rows) > 0)
      filter_table(f$rows, 10, selection = "none", dropdowns = character())
    })

    output$columns <- DT::renderDT({
      shiny::req(result(), input$table)
      columns <- safely(qc_table_columns(result(), input$table))
      shiny::req(!failed(columns))
      plain_table(columns, 15)
    })
  })
}
