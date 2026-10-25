# =============================================================================
# The app (step 5): qc_app() opens it in the browser.
#
#   1 Load -> 2 Overview -> 3 Data issues -> 4 Metadata -> 5 Map -> 6 Explore
#   -> 7 Download, and the Help menu: Checks, Manual, About the tool
#
#   60_app.R           launcher, page layout, things all screens use
#   61_app_load.R      file or web address, column mapping, options, which checks, run
#   62_app_overview.R  verdict, essentials, data checks per table, could not run
#   63_app_issues.R    per table: problems -> their records (with their values) -> one record
#   64_app_map.R       positions with problems on a map
#   65_app_download.R  Excel / CSV / JSON downloads
#   66_app_metadata.R  the metadata (EML): FAIR letters and the metadata issues
#   67_app_explore.R   time, taxa, measurements, distributions, relationships, sexes, events
#   69_app_tables.R    Explore > Tables: the tables, their links, their rows (with a search)
#   68_app_help.R      Help: find a check, the manual, about the tool
#
# Each screen is a Shiny module: <screen>_ui(id) and <screen>_server(id, result),
# where result is the qc_check() result shared by all screens.
# If something goes wrong on a screen, that screen shows the message and the
# rest of the app keeps working (see safely()).
# The check runs in the app's own R session (with a progress bar). With several
# users on one server, a big dataset makes the others wait: see
# docs/design.md, section 6.
# The packages of the app (shiny, bslib, DT, leaflet, zip, plotly, markdown) are installed with the tool (Imports).
# =============================================================================

#' Open the QC Tool app in the browser
#'
#' @param port Optional port number.
#' @param launch_browser Open the app in the browser (default `TRUE`).
#' @param max_upload_mb The largest file that can be uploaded, in MB. Default:
#'   the option `qctool.max_upload_mb` (3000). The other limits (download,
#'   unzipped size) are options too: see R/08_options.R.
#' @param stop_on_close When the browser tab is closed or loses its connection,
#'   stop the app, so R is free again (default `TRUE`). Use `FALSE` when several
#'   people share one running app.
#' @return Runs until the app is closed.
#' @export
qc_app <- function(port = NULL, launch_browser = TRUE, max_upload_mb = qc_option("qctool.max_upload_mb"), stop_on_close = TRUE) {
  needed <- c("shiny", "bslib", "DT", "leaflet", "zip", "plotly", "markdown")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("The app needs these packages: ", paste(missing, collapse = ", "), "\nInstall them with install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
  }
  old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2, qctool.stop_on_close = stop_on_close)
  on.exit(options(old), add = TRUE)
  # Build the theme files before the first visitor asks for them (otherwise the
  # very first page load can miss Bootstrap's JavaScript).
  invisible(bslib::bs_theme_dependencies(app_theme()))
  shiny::runApp(shiny::shinyApp(app_ui(), app_server), port = if (is.null(port)) getOption("shiny.port") else port,
                launch.browser = launch_browser)
}

# In the page head:
# - every 30 seconds a small sign of life to R. Proxies (e.g. on an RStudio
#   Server) close connections that stay silent for a few minutes; this keeps
#   the app connected while someone reads a report.
# - the grey screen after a lost connection says what to do.
keep_alive_head <- function() {
  shiny::tags$head(
    shiny::tags$script(shiny::HTML(
      "setInterval(function () { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('keep_alive', Date.now()); }, 30000);")),
    shiny::tags$style(shiny::HTML(
      "#shiny-disconnected-overlay::after { content: 'The connection to R was lost. In R, run qc_app() again, then reload this page.';",
      " position: fixed; top: 40%; left: 50%; transform: translateX(-50%); width: min(90vw, 26em); text-align: center;",
      " background: #fff; padding: 1em 1.5em;",
      " border-radius: 6px; font-size: 1.1em; box-shadow: 0 2px 8px rgba(0,0,0,0.3); }")))
}

app_theme <- function() bslib::bs_theme(version = 5, primary = "#1F5C99")

app_ui <- function() {
  bslib::page_navbar(
    id = "screen",
    title = "QC Tool (Beta)",
    window_title = "QC Tool (Beta)",
    theme = app_theme(),
    fillable = FALSE,
    header = keep_alive_head(),
    bslib::nav_panel("1 Load", value = "load", load_ui("load")),
    bslib::nav_panel("2 Overview", value = "overview", overview_ui("overview")),
    bslib::nav_panel("3 Data issues", value = "issues", issues_ui("issues")),
    bslib::nav_panel("4 Metadata", value = "metadata", metadata_ui("metadata")),
    bslib::nav_panel("5 Map", value = "map", map_ui("map")),
    bslib::nav_panel("6 Explore", value = "explore", explore_ui("explore")),
    bslib::nav_panel("7 Download", value = "download", download_ui("download")),
    bslib::nav_spacer(),
    bslib::nav_menu(
      "Help", align = "right",
      bslib::nav_panel("Checks: find and read about every check", value = "checks", checks_ui("checks")),
      bslib::nav_panel("Manual", value = "manual", manual_ui("manual")),
      bslib::nav_panel("About the tool", value = "about", about_tool_ui("about")))
  )
}

app_server <- function(input, output, session) {
  # Tab closed or connection lost: stop the app, so R is free for a new qc_app().
  if (isTRUE(getOption("qctool.stop_on_close"))) session$onSessionEnded(function() shiny::stopApp())
  result <- shiny::reactiveVal(NULL)
  load_server("load", result)
  shiny::observeEvent(result(), bslib::nav_select("screen", "overview"))
  overview_server("overview", result)
  issues_server("issues", result)
  metadata_server("metadata", result)
  map_server("map", result)
  explore_server("explore", result)
  download_server("download", result)
  checks_server("checks", result)
  manual_server("manual")
  about_tool_server("about", result)
}

# --- Things all screens use ---------------------------------------------------------

severity_colours <- c(error = "#C0392B", warning = "#D68910", info = "#2471A3")

# Run code for a screen. An error comes back as a value (shown with
# error_alert() on that screen) instead of closing the whole app. req() is
# let through: it means "nothing to show yet", not an error.
safely <- function(expr) {
  tryCatch(expr, error = function(e) if (inherits(e, "shiny.silent.error")) stop(e) else e)
}

failed <- function(x) inherits(x, "error")

error_alert <- function(e, what) {
  alert(shiny::tagList(shiny::strong(paste(what, "could not be shown.")), " ", conditionMessage(e),
                       shiny::br(), "The other screens still work. Please send this message and the dataset to the data management team."),
        "danger")
}

# Shown on a screen while no dataset has been checked yet.
nothing_yet <- function() {
  bslib::card(class = "mt-3", bslib::card_body(
    shiny::p(class = "lead mb-0", "No dataset checked yet."),
    shiny::p(class = "text-muted mb-0", "Choose a file or web address on screen 1 (Load) and press 'Check the dataset'.")))
}

alert <- function(content, type = "info") shiny::div(class = paste0("alert alert-", type, " mt-3 mb-0"), content)

# The severity choice used on several screens. Errors block the harvest; the
# others are for the data managers to judge, so errors are shown first.
severity_input <- function(id, selected = "error", inline = FALSE) {
  shiny::checkboxGroupInput(id, "Show", inline = inline, selected = selected,
                            choices = c("errors (block the harvest)" = "error", "warnings" = "warning", "info" = "info"))
}

# A table with a filter box above every column (dropdowns for columns with few
# different values) and paging on the server.
filter_table <- function(data, page_length = 10, selection = "single", dropdowns = c("severity", "check", "table", "field", "status", "where")) {
  data <- as.data.frame(utf8_safe(data))
  for (column in intersect(dropdowns, names(data))) data[[column]] <- factor(data[[column]])
  links <- web_links(data)
  DT::datatable(links$data, rownames = FALSE, selection = selection, filter = list(position = "top", clear = FALSE),
                escape = links$escape,
                fillContainer = FALSE,   # its own height: no small scrolling box inside a card
                options = list(pageLength = page_length, scrollX = TRUE, searchHighlight = TRUE, autoWidth = FALSE))
}

# A small table shown as it is: for short lists.
plain_table <- function(data, page_length = 10) {
  links <- web_links(as.data.frame(utf8_safe(data)))
  DT::datatable(links$data, rownames = FALSE, selection = "none", fillContainer = FALSE, escape = links$escape,
                options = list(dom = if (nrow(data) > page_length) "tp" else "t", pageLength = page_length, scrollX = TRUE))
}

# Web addresses in a table become links that open in a new tab, also inside a
# longer text. All other text stays escaped (shown as text, never run as
# HTML). Returns the data and the columns DT must still escape.
web_links <- function(data) {
  has_link <- function(x) is.character(x) && any(grepl("https?://", x, useBytes = TRUE), na.rm = TRUE)
  link_columns <- names(data)[vapply(data, has_link, logical(1))]
  for (column in link_columns) {
    x <- data[[column]]
    shown <- linkify(htmltools::htmlEscape(x))
    shown[is.na(x)] <- NA_character_
    data[[column]] <- shown
  }
  list(data = data, escape = if (length(link_columns)) setdiff(names(data), link_columns) else TRUE)
}

# In escaped text, every web address becomes a link. Punctuation after it
# stays outside the link; an address cut short ('…') is left as text.
linkify <- function(text) {
  # An address ends at a space, a quote, or an escaped < > " ' (&lt; &gt; &quot; &#39;).
  found <- gregexpr("https?://(?:(?!&lt;|&gt;|&quot;|&#39;)[^[:space:]<>\"'])+", text, perl = TRUE)
  regmatches(text, found) <- lapply(regmatches(text, found), function(urls) {
    if (!length(urls)) return(urls)
    tail <- ifelse(grepl("[.,;:)\\]]+$", urls, perl = TRUE), sub("^.*?([.,;:)\\]]+)$", "\\1", urls, perl = TRUE), "")
    address <- substr(urls, 1, nchar(urls) - nchar(tail))
    cut_short <- grepl("…", urls, fixed = TRUE)
    ifelse(cut_short, urls, sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>%s', address, address, tail))
  })
  text
}

# Text sent to the browser must be valid UTF-8 (invalid text makes it drop its
# connection): utf8_safe() in R/40_helpers.R. The readers already repair the
# data (repair_invalid_text); utf8_safe() here is a second safety net.

# Colour a severity column in a DT table.
colour_severity <- function(table, column = "severity") {
  DT::formatStyle(table, column, color = DT::styleEqual(names(severity_colours), unname(severity_colours)), fontWeight = "bold")
}

# The check titles, to show next to (or instead of) the check ids.
titles_of_checks <- function(result) stats::setNames(result$checks$title, result$checks$check_id)

# The data tables of a result in Darwin Core order, with a readable name.
table_labels <- c(event = "Event", occurrence = "Occurrence", emof = "eMoF (measurements)", mof = "MoF (measurements)",
                  dna = "DNA derived data", eml = "Metadata (EML)")

# Metadata problems: those of the checks in the group 'metadata' (the EML, and
# the licence also when it is a column of the data). Screen 4 shows them; the
# data screens show all others.
metadata_problems <- function(result) {
  ids <- result$checks$check_id[result$checks$group == "metadata"]
  result$issues_grouped[check_id %chin% ids | table == "eml"]
}

data_problems <- function(result) {
  ids <- result$checks$check_id[result$checks$group == "metadata"]
  result$issues_grouped[!check_id %chin% ids & table != "eml"]
}

data_tables_of <- function(result) {
  present <- unique(c(result$tables$table, data_problems(result)$table))
  present <- setdiff(present, "eml")
  c(intersect(names(table_labels), present), setdiff(present, names(table_labels)))
}

table_label <- function(table_name) ifelse(table_name %in% names(table_labels), table_labels[table_name], table_name)

# Issues per check and table, by severity (a problem without records counts as 1).
issue_counts <- function(problems) {
  problems[, .(errors = sum(pmax(records, 1L) * (severity == "error")),
               warnings = sum(pmax(records, 1L) * (severity == "warning")),
               info = sum(pmax(records, 1L) * (severity == "info"))), by = .(check_id, table)]
}
