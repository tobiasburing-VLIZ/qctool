# =============================================================================
# App: the Help menu
#   Checks          find a check by words; everything about it (what, why, how,
#                   settings, lists, online service, where in the code, result)
#   Manual          inst/manual/manual.md
#   About the tool  version, disclaimer, contact, useful links
#                   (inst/reference/about_the_tool.csv: edit it there)
# =============================================================================

# --- Checks --------------------------------------------------------------------------

checks_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "mt-3", fill = FALSE,
      bslib::card_header("Find a check"),
      shiny::textInput(ns("search"), NULL, width = "100%",
                       placeholder = "Words to look for, e.g. 'NERC label', 'land', 'ORCID', 'duplicate', 'eventDate'"),
      shiny::helpText("All words must appear in the title, id, description, fields, settings, value sources or notes of a check.",
                      "Click a check to see everything about it. Severities come from the rules workbook",
                      shiny::code(normalizePath(default_rules_path(), winslash = "/")),
                      "(column severity of the sheet Checks; Help > About the tool to download it; manual section 7.1)."),
      DT::DTOutput(ns("list"), fill = FALSE)),
    bslib::card(fill = FALSE,
      bslib::card_header(shiny::textOutput(ns("detail_title"), inline = TRUE)),
      shiny::uiOutput(ns("detail"))))
}

checks_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    catalogue <- catalogue_of_checks()                     # read once per visitor
    search <- shiny::debounce(shiny::reactive(input$search), 400)
    found <- shiny::reactive(find_checks(catalogue, search()))

    output$list <- DT::renderDT({
      shown <- found()[, .(title, check_id, group, severity, implemented, internet, what = shorten(what, 160))]
      colour_severity(filter_table(shown, 15, dropdowns = c("group", "severity", "implemented")))
    })

    chosen <- shiny::reactive({
      selected <- input$list_rows_selected
      shiny::req(length(selected) == 1)
      found()[selected]
    })

    output$detail_title <- shiny::renderText({
      if (length(input$list_rows_selected) != 1) return("Check: choose one above")
      sprintf("%s (%s)", chosen()$title, chosen()$check_id)
    })

    output$detail <- shiny::renderUI({
      chk <- chosen()
      r <- result()
      in_this_run <- if (is.null(r)) "no dataset checked yet" else {
        status <- r$checks[check_id == chk$check_id]
        if (!nrow(status)) "not run in this check (not chosen)" else {
          sprintf("%s%s%s", status$status, if (status$issues > 0) sprintf(", %s issues", format(status$issues, big.mark = ",")) else "",
                  if (nzchar(status$reason)) paste0(" (", status$reason, ")") else "")
        }
      }
      rows <- list(
        "What it checks" = chk$what, "Why" = chk$why, "Severity (rules workbook)" = chk$severity,
        "Where GBIF / OBIS differ" = chk$other_standards, "Group" = chk$group, "Tables" = chk$tables, "Fields" = chk$fields,
        "Settings (rules workbook, sheet Checks)" = chk$settings, "Runs after" = chk$depends_on, "Values from" = chk$reference_list,
        "Online service" = if (nzchar(chk$internet)) chk$internet else "none (works offline)",
        "In this dataset" = in_this_run, "Implemented" = chk$implemented, "Notes" = chk$notes, "Code" = chk$code)
      rows <- rows[!vapply(rows, function(v) is.null(v) || is.na(v) || !nzchar(v), logical(1))]
      shiny::tags$dl(class = "row mx-2 mt-2 mb-0", lapply(names(rows), function(name) {
        shiny::tagList(shiny::tags$dt(class = "col-sm-3", name), shiny::tags$dd(class = "col-sm-9", rows[[name]]))
      }))
    })
  })
}

# --- Manual --------------------------------------------------------------------------

manual_ui <- function(id) {
  output_id <- shiny::NS(id, "text")
  shiny::tagList(
    shiny::tags$style(sprintf(paste(
      "#%1$s table { width: auto; margin: 0.5rem 0 1.25rem; border-collapse: collapse; }",
      "#%1$s th, #%1$s td { border: 1px solid #dee2e6; padding: 0.3rem 0.6rem; vertical-align: top; }",
      "#%1$s th { background: #f3f6f9; }",
      "#%1$s h2 { margin-top: 2rem; } #%1$s pre { background: #f6f8fa; padding: 0.75rem; }",
      "#%1$s img { max-width: 100%%; height: auto; margin: 0.5rem 0 1rem; }"), output_id)),
    bslib::card(class = "mt-3 mb-4", fill = FALSE, bslib::card_body(shiny::uiOutput(output_id))))
}

manual_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$text <- shiny::renderUI({
      file <- system.file("manual", "manual.md", package = "qctool")
      if (!nzchar(file)) return(alert("The manual (inst/manual/manual.md) was not found.", "warning"))
      # Pictures of the manual (e.g. architecture.png) are served from its folder.
      shiny::addResourcePath("qctool-manual", dirname(file))
      html <- as.character(shiny::includeMarkdown(file))
      shiny::HTML(gsub('src="([^":/]+\\.(png|jpg|svg))"', 'src="qctool-manual/\\1"', html))
    })
  })
}

# --- About the tool ------------------------------------------------------------------

about_tool_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

about_tool_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$rules_file <- shiny::downloadHandler(
      filename = function() "qc_rules.xlsx",
      content = function(file) file.copy(default_rules_path(), file))

    output$page <- shiny::renderUI({
      about <- reference_table("about_the_tool.csv")
      value_of <- function(label) about$value[about$label == label][1]
      links <- about[section == "link"]
      rules <- qc_read_rules()
      bslib::layout_columns(
        col_widths = c(7, 5), class = "mt-3",
        bslib::card(
          bslib::card_header(sprintf("QC Tool (Beta), version %s", utils::packageVersion("qctool"))),
          bslib::card_body(
            shiny::p(value_of("What it does")),
            shiny::tags$dl(class = "row mb-0",
              shiny::tags$dt(class = "col-sm-4", "Developed by"), shiny::tags$dd(class = "col-sm-8", value_of("Developed by")),
              shiny::tags$dt(class = "col-sm-4", "Contact"),
              shiny::tags$dd(class = "col-sm-8", shiny::a(href = paste0("mailto:", value_of("Contact")), value_of("Contact"))),
              shiny::tags$dt(class = "col-sm-4", "Rules workbook"),
              shiny::tags$dd(class = "col-sm-8",
                shiny::code(normalizePath(rules$path, winslash = "/")), sprintf(" (%d checks). ", nrow(rules$checks)),
                "Change severities and settings there in Excel; the app reads it at every check (manual, section 7.1). ",
                shiny::downloadLink(session$ns("rules_file"), "Download this workbook"),
                if (!is.null(result()) && !identical(normalizePath(result()$run$rules_file, mustWork = FALSE), normalizePath(rules$path)))
                  shiny::div(class = "small text-muted", "The last check used another workbook: ", basename(result()$run$rules_file))),
              shiny::tags$dt(class = "col-sm-4", "Running on"),
              shiny::tags$dd(class = "col-sm-8", sprintf("R %s; data.table %s; shiny %s", getRversion(), utils::packageVersion("data.table"),
                                                       utils::packageVersion("shiny")))),
            alert(shiny::tagList(shiny::strong("Disclaimer. "), value_of("Disclaimer")), "secondary"))),
        bslib::card(
          bslib::card_header("Useful links"),
          bslib::card_body(shiny::tags$ul(lapply(seq_len(nrow(links)), function(i) {
            shiny::tags$li(shiny::a(href = links$value[i], target = "_blank", rel = "noopener", links$label[i]))
          })),
          shiny::helpText("Edit these texts and links in inst/reference/about_the_tool.csv."))))
    })
  })
}
