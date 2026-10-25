# =============================================================================
# App screen 7: Download (Excel, CSV, JSON).
# (The check descriptions are on the Help > Checks screen, 68_app_help.R.)
# =============================================================================

download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("nothing")),
    bslib::layout_column_wrap(
      width = 1 / 3, class = "mt-3",
      bslib::card(
        bslib::card_header("Excel report"),
        shiny::p("Summary, checks, problems and issues with record ID and line. Best for reading and sharing."),
        shiny::numericInput(ns("max_rows"), "Most issue rows in Excel", value = 100000, min = 1000, max = excel_max_rows, step = 50000),
        shiny::downloadButton(ns("xlsx"), "Download Excel", class = "btn-primary")),
      bslib::card(
        bslib::card_header("CSV files (zip)"),
        shiny::p("All issues, also when there are millions. For further work in R, Python or a database."),
        shiny::radioButtons(ns("sep"), "Separator", inline = TRUE, choices = c("comma (,)" = ",", "semicolon (;)" = ";")),
        shiny::helpText("Semicolon opens directly in Excel with Belgian, Dutch or German settings."),
        shiny::downloadButton(ns("csv"), "Download CSV", class = "btn-primary")),
      bslib::card(
        bslib::card_header("JSON"),
        shiny::p("For other programs: run information, checks, problems (and all issues when they fit)."),
        shiny::downloadButton(ns("json"), "Download JSON", class = "btn-primary")))
  )
}

download_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nothing <- shiny::renderUI(if (is.null(result())) nothing_yet())

    # "dwca-biofun_2009-v1.9_qc" for the file names
    file_stem <- shiny::reactive({
      name <- tools::file_path_sans_ext(shiny::req(result())$run$source_name)
      paste0(gsub("[^A-Za-z0-9._-]+", "_", name), "_qc")
    })

    output$xlsx <- shiny::downloadHandler(
      filename = function() paste0(file_stem(), ".xlsx"),
      content = function(file) {
        shiny::withProgress(message = "Writing Excel", qc_export_xlsx(result(), file, max_issue_rows = input$max_rows))
      })

    output$csv <- shiny::downloadHandler(
      filename = function() paste0(file_stem(), "_csv.zip"),
      content = function(file) {
        folder <- tempfile("csv_")
        shiny::withProgress(message = "Writing CSV files", qc_export_csv(result(), folder, sep = input$sep))
        zip::zipr(file, list.files(folder, full.names = TRUE))
      })

    output$json <- shiny::downloadHandler(
      filename = function() paste0(file_stem(), ".json"),
      content = function(file) qc_export_json(result(), file))
  })
}
