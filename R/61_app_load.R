# =============================================================================
# App screen 1: Load. A file (archive or table) or a web address; for tables
# the column mapping; options (online services, own rules); run the check.
#
# A web address can lead to an archive or to a table. The first press on
# 'Check the dataset' downloads it (download_dataset(), R/14_read_url.R). An
# archive is checked at once; a table first shows its column mapping, like an
# uploaded table, and is checked at the next press. Only http(s) addresses are
# accepted: a path on the server's disk is refused.
# =============================================================================

load_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(8, 4), class = "mt-3",
    bslib::card(
      bslib::card_header("What do you want to check?"),
      bslib::navset_underline(
        id = ns("source"),
        bslib::nav_panel("A file", value = "file",
          shiny::p(class = "text-muted mt-3", "A Darwin Core Archive (.zip, as downloaded from an IPT), or a simple table (.csv, .txt, .xlsx)."),
          shiny::fileInput(ns("file"), NULL, width = "100%", buttonLabel = "Choose a file...",
                           accept = c(".zip", ".csv", ".txt", ".tsv", ".xlsx", ".xls"))),
        bslib::nav_panel("A web address", value = "url",
          shiny::p(class = "text-muted mt-3", "The address of an IPT resource, of a dataset page on GBIF or OBIS, or a link to",
                   "an archive (.zip) or a table (.csv, .txt, .xlsx). A table gets its column mapping after the first press on",
                   "'Check the dataset'."),
          shiny::textInput(ns("url"), NULL, width = "100%", placeholder = "https://ipt.vliz.be/upload/resource?r=..."))),
      shiny::uiOutput(ns("sheet_choice")),
      shiny::uiOutput(ns("mapping"))
    ),
    bslib::card(
      bslib::card_header("Options"),
      shiny::checkboxInput(ns("online"), "Use online services (WoRMS, NERC, OBIS)", value = TRUE),
      shiny::uiOutput(ns("internet")),
      shiny::fileInput(ns("rules"), "Own rules workbook (optional)", accept = ".xlsx", buttonLabel = "Choose..."),
      shiny::helpText("Leave empty to use the EMODnet rules that come with the tool."),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Only some checks (optional)",
          shiny::selectizeInput(ns("groups"), "Groups of checks", choices = group_choices(), multiple = TRUE,
                                options = list(placeholder = "all groups")),
          shiny::selectizeInput(ns("single"), "Single checks", choices = single_check_choices(), multiple = TRUE,
                                options = list(placeholder = "none")),
          shiny::helpText("Empty: all checks. Checks that these depend on are added automatically."))),
      shiny::actionButton(ns("run"), "Check the dataset", class = "btn-primary btn-lg w-100 mt-2"),
      shiny::uiOutput(ns("message"))
    )
  )
}

load_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    message <- shiny::reactiveVal(NULL)
    output$message <- shiny::renderUI(message())

    # The uploaded file, under its own name (the report shows the file name).
    uploaded <- shiny::reactive({
      shiny::req(input$file)
      folder <- tempfile("upload_")
      dir.create(folder)
      path <- file.path(folder, input$file$name)
      file.copy(input$file$datapath, path)
      path
    })

    # The dataset behind the web address, once downloaded (download_dataset()).
    fetched <- shiny::reactiveVal(NULL)
    fetched_now <- shiny::reactive({
      f <- fetched()
      if (!is.null(f) && identical(input$source, "url") && identical(f$url, trimws(input$url))) f else NULL
    })

    # The table to map: an uploaded table, or a table behind the web address. NULL for archives.
    table_path <- shiny::reactive({
      if (identical(input$source, "url")) {
        f <- fetched_now()
        return(if (!is.null(f) && f$kind %in% table_kinds) f$file else NULL)
      }
      if (identical(input$source, "file") && !is.null(input$file) &&
          grepl("\\.(csv|txt|tsv|xlsx|xls)$", input$file$name, ignore.case = TRUE)) uploaded() else NULL
    })
    is_table <- shiny::reactive(!is.null(table_path()))
    is_excel <- shiny::reactive(is_table() && grepl("\\.xlsx?$", table_path(), ignore.case = TRUE))

    output$sheet_choice <- shiny::renderUI({
      if (!is_excel()) return(NULL)
      sheets <- safely(readxl::excel_sheets(table_path()))
      if (failed(sheets)) return(error_alert(sheets, "The sheets of this Excel file"))
      shiny::selectInput(session$ns("sheet"), "Sheet", choices = utf8_safe(sheets))
    })

    # The columns of a table, with a few example values each.
    columns <- shiny::reactive({
      shiny::req(is_table())
      if (is_excel()) shiny::req(input$sheet)
      safely({
        raw <- if (is_excel()) read_excel_as_text(table_path(), input$sheet) else read_text_table(table_path())
        examples <- vapply(raw, function(x) shorten(paste(utils::head(unique(without_na(x)), 3), collapse = " | "), 60), character(1))
        # 'column' stays as in the file (the mapping uses it); 'shown' is safe to send to the browser.
        data.table(column = names(raw), shown = utf8_safe(names(raw)), examples = utf8_safe(unname(examples)))
      })
    })

    output$mapping <- shiny::renderUI({
      if (!is_table()) return(NULL)
      ns <- session$ns
      table_columns <- columns()
      if (failed(table_columns)) return(error_alert(table_columns, "The columns of this table"))
      proposal <- safely(qc_suggest_mapping(table_columns$column))
      if (failed(proposal)) return(error_alert(proposal, "The proposed column mapping"))
      choices <- c("(not used)" = "", sort(term_table()$term))
      rows <- lapply(seq_len(nrow(table_columns)), function(i) {
        bslib::layout_columns(
          col_widths = c(4, 4, 4), class = "align-items-center border-bottom py-1", gap = "0.5rem",
          shiny::div(shiny::strong(table_columns$shown[i]), shiny::div(class = "small text-muted text-truncate", table_columns$examples[i])),
          shiny::selectizeInput(ns(paste0("term_", i)), NULL, choices = choices, width = "100%",
                                selected = if (is.na(proposal$term[i])) "" else proposal$term[i]),
          shiny::div(class = "small text-muted", utf8_safe(mapping_explained(proposal$how[i], proposal$note[i]))))
      })
      shiny::tagList(
        shiny::h5(class = "mt-3", "Which Darwin Core term is each column?"),
        shiny::p(class = "text-muted", "The tool proposes a term for every column. Check the proposals, change them where",
                 "needed, and choose '(not used)' for columns that should be left out."),
        shiny::radioButtons(ns("table_type"), "Each row of the table is", inline = TRUE,
                            choices = c("an occurrence" = "occurrence", "an event (a sample)" = "event")),
        rows)
    })

    output$internet <- shiny::renderUI({
      if (!isTRUE(input$online)) {
        return(shiny::helpText("Offline: only answers stored earlier are used; checks that need more are reported as 'not performed'."))
      }
      if (curl::has_internet()) shiny::helpText("Internet connection: yes.")
      else shiny::helpText(class = "text-danger", "No internet connection: the online checks use stored answers only.")
    })

    shiny::observeEvent(input$run, {
      message(NULL)
      from_web <- identical(input$source, "url")
      source <- if (from_web) trimws(input$url) else if (!is.null(input$file)) uploaded() else ""
      if (!nzchar(source)) {
        message(alert("Choose a file or give a web address first.", "warning"))
        return()
      }
      if (from_web && !is_url(source)) {
        message(alert("Give a web address that starts with http:// or https://. For a file on your computer, use 'A file'.", "warning"))
        return()
      }

      # The rules workbook (own or the tool's) decides which checks 'Only some checks' means.
      rules <- if (is.null(input$rules)) NULL else input$rules$datapath
      all_checks <- tryCatch(qc_read_rules(if (is.null(rules)) default_rules_path() else rules)$checks, error = function(e) e)
      if (inherits(all_checks, "error")) {
        message(alert(paste("The rules workbook could not be read:", conditionMessage(all_checks)), "danger"))
        return()
      }
      unknown <- setdiff(input$single, all_checks$check_id)
      if (length(unknown)) {
        message(alert(paste("These checks are not in the rules workbook you chose:", paste(unknown, collapse = ", ")), "warning"))
        return()
      }
      checks <- unique(c(all_checks$check_id[all_checks$group %in% input$groups], input$single))
      if (!length(checks)) checks <- NULL

      # A web address: download it. An archive is downloaded again at every
      # check; a table only once, because its column mapping comes in between.
      web <- NULL
      if (from_web) {
        web <- fetched_now()
        if (is.null(web) || !web$kind %in% table_kinds) {
          web <- shiny::withProgress(message = "Downloading the dataset", value = 0.1,
                                     tryCatch(download_dataset(source), error = function(e) e))
          if (inherits(web, "error")) {
            message(alert(paste("The dataset could not be downloaded:", conditionMessage(web)), "danger"))
            return()
          }
          if (!is.null(fetched())) unlink(fetched()$folder, recursive = TRUE)
          fetched(web)
          if (web$kind %in% table_kinds) {
            message(alert(paste("This address leads to a table. Check below which Darwin Core term each column is,",
                                "then press 'Check the dataset' again."), "info"))
            return()
          }
        }
      }

      mapping <- NULL
      sheet <- NULL
      table_type <- "occurrence"
      if (is_table()) {
        if (failed(columns())) {
          message(error_alert(columns(), "The table"))
          return()
        }
        table_columns <- columns()$column
        terms <- vapply(seq_along(table_columns), function(i) {
          term <- input[[paste0("term_", i)]]
          if (is.null(term) || !nzchar(term)) NA_character_ else term
        }, character(1))
        mapping <- stats::setNames(terms, table_columns)
        sheet <- input$sheet
        if (!is.null(input$table_type)) table_type <- input$table_type
      }

      checked <- shiny::withProgress(message = "Checking", value = 0, {
        shiny::setProgress(0.01, detail = "reading the dataset")
        tryCatch({
          what <- if (is.null(web)) source else read_downloaded(web, mapping = mapping, sheet = sheet, table_type = table_type)
          qc_check(what, checks = checks, rules = rules, mapping = mapping, sheet = sheet, table_type = table_type,
                   online = isTRUE(input$online),
                   progress = function(done, total, title) shiny::setProgress(done / max(total, 1), detail = title))
        }, error = function(e) e)
      })
      if (!is.null(web) && !web$kind %in% table_kinds) {   # a downloaded archive is not needed any more
        unlink(web$folder, recursive = TRUE)
        fetched(NULL)
      }
      if (inherits(checked, "error")) {
        message(alert(paste("The check could not be done:", conditionMessage(checked)), "danger"))
        return()
      }
      result(checked)
      message(alert(sprintf("Checked in %s seconds. The results are on the next screens.", checked$run$duration_s), "success"))
    })
  })
}

# The groups of checks of the rules workbook, with readable names (group_names: R/04_result.R).
group_choices <- function() {
  groups <- intersect(names(group_names), unique(qc_read_rules()$checks$group))
  stats::setNames(groups, group_names[groups])
}

single_check_choices <- function() {
  checks <- qc_read_rules()$checks[severity != "off"][order(group, title)]
  stats::setNames(checks$check_id, paste0(checks$title, " (", checks$check_id, ")"))
}

# How the tool found the proposed term, in words.
mapping_explained <- function(how, note) {
  text <- switch(how,
    "exact" = "same name as the term",
    "capitals" = "same name, other capitals",
    "synonym" = "a known other name for this term",
    "similar" = "a similar name: please check",
    "your mapping" = "your choice",
    "none" = if (!is.na(note) && nzchar(note)) "not used" else "no matching term found",
    how)
  if (!is.na(note) && nzchar(note)) paste0(text, " (", note, ")") else text
}
