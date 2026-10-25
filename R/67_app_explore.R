# =============================================================================
# App screen 6: Explore. A look at the data, for a person to judge:
#   Tables        the tables, how they are linked, their rows (69_app_tables.R)
#   Time          records per year or month, the metadata time span, the
#                 middle 95 % of the dates
#   Taxa          the most common taxa, all taxa with WoRMS details and issues
#   Measurements  per type and unit: records, numeric range, text values, NERC label
#   Events        the event hierarchy per level
# The summaries are in 46_explore.R and 47_explore_more.R; each is made only
# when its tab is opened. Charts: plotly: hover over a bar or point
# to read its values; zoom by dragging; double-click to zoom out.
# =============================================================================

# Box plot of a variable: one box for all records and one per taxon. Only the
# box numbers and the values beyond the whiskers (at most 300 per box) go to
# the browser, so it stays quick with millions of values.
box_chart <- function(values, on_log, label) {
  if (on_log) values <- values[value > 0][, value := log10(value)]
  shiny::req(nrow(values) > 0)
  boxes <- utf8_safe(qc_box_stats(values))
  boxes[, name := sprintf("%s (n = %s)", group, format(n, big.mark = ",", trim = TRUE))]
  beyond <- rbindlist(lapply(seq_len(nrow(boxes)), function(i) {
    v <- if (i == 1) values$value else values[taxon %chin% boxes$group[i], value]
    v <- v[v < boxes$lower[i] | v > boxes$upper[i]]
    if (length(v) > 300) v <- v[sample.int(length(v), 300)]
    data.table(name = rep(boxes$name[i], length(v)), value = v)
  }))
  chart <- plotly::plot_ly(type = "box", orientation = "h", y = boxes$name, q1 = boxes$q1, median = boxes$median, q3 = boxes$q3,
                           lowerfence = boxes$lower, upperfence = boxes$upper, fillcolor = "rgba(31, 92, 153, 0.25)",
                           line = list(color = "#1F5C99"), name = "box", showlegend = FALSE)
  if (nrow(beyond)) {
    chart <- plotly::add_trace(chart, type = "scatter", mode = "markers", x = beyond$value, y = beyond$name, inherit = FALSE,
                               marker = list(color = "#D68910", size = 5), name = "beyond the whiskers", showlegend = FALSE,
                               text = sprintf("%s: %s", htmltools::htmlEscape(beyond$name), signif(beyond$value, 5)), hoverinfo = "text")
  }
  chart_layout(plotly::layout(chart, yaxis = list(categoryorder = "array", categoryarray = rev(boxes$name), title = "")),
               x = if (on_log) paste0("log10(", label, ")") else label)
}

# Common look of the charts: axis titles, white background, a small toolbar.
chart_layout <- function(chart, x = "", y = "") {
  chart <- plotly::layout(chart, xaxis = list(title = x), yaxis = list(title = y), hoverlabel = list(align = "left"),
                          margin = list(t = 30), font = list(size = 13))
  plotly::config(chart, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

explore_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("nothing")),
    shiny::div(class = "mt-3", bslib::navset_card_underline(
      bslib::nav_panel("Tables", tables_ui(ns("tables"))),
      bslib::nav_panel("Time", shiny::uiOutput(ns("time_note")), plotly::plotlyOutput(ns("time_plot"), height = "380px")),
      bslib::nav_panel("Taxa", shiny::uiOutput(ns("taxa_note")), plotly::plotlyOutput(ns("taxa_plot"), height = "440px"),
                       DT::DTOutput(ns("taxa_table"), fill = FALSE)),
      bslib::nav_panel("Measurements", shiny::uiOutput(ns("emof_note")), DT::DTOutput(ns("emof_table"), fill = FALSE)),
      bslib::nav_panel("Distributions",
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::div(class = "mt-2",
            shiny::selectInput(ns("dist_variable"), "Variable", choices = NULL),
            shiny::selectizeInput(ns("dist_taxon"), "Taxon", choices = NULL, options = list(placeholder = "all taxa")),
            shiny::radioButtons(ns("dist_chart"), "Chart", choices = c("histogram (bars)" = "histogram", "box plot" = "box")),
            shiny::conditionalPanel(sprintf("input['%s'] == 'histogram'", ns("dist_chart")),
              shiny::sliderInput(ns("dist_bins"), "Number of bars", min = 10, max = 100, value = 40)),
            shiny::conditionalPanel(sprintf("input['%s'] == 'box'", ns("dist_chart")),
              shiny::helpText("All taxa: one box for all records and one per taxon (the 15 with most values).",
                              "Box: the middle 50 %; line: the median; whiskers: up to 1.5 times the box length; dots: values beyond.")),
            shiny::checkboxInput(ns("dist_log"), "Logarithmic scale (log10)", value = FALSE)),
          shiny::div(shiny::uiOutput(ns("dist_note")), plotly::plotlyOutput(ns("dist_plot"), height = "380px")))),
      bslib::nav_panel("Relationships",
        bslib::layout_columns(
          col_widths = c(3, 9),
          shiny::div(class = "mt-2",
            shiny::selectInput(ns("rel_x"), "x (e.g. a length)", choices = NULL),
            shiny::selectInput(ns("rel_y"), "y (e.g. a weight)", choices = NULL),
            shiny::selectizeInput(ns("rel_taxon"), "Taxon (best: one at a time)", choices = NULL, options = list(placeholder = "all taxa")),
            shiny::radioButtons(ns("rel_model"), "Model",
                                choices = c("power: y = a x^b (length-weight)" = "power", "exponential: y = a e^(bx)" = "exponential",
                                            "linear: y = a + bx" = "linear")),
            shiny::numericInput(ns("rel_threshold"), "Far from the curve: more than ... typical deviations", value = 3, min = 1, max = 10, step = 0.5),
            shiny::helpText("A crude model to find odd values by eye, not a scientific fit.")),
          shiny::div(shiny::uiOutput(ns("rel_note")), plotly::plotlyOutput(ns("rel_plot"), height = "420px"),
                     DT::DTOutput(ns("rel_table"), fill = FALSE)))),
      bslib::nav_panel("Sexes", shiny::uiOutput(ns("sex_note")), plotly::plotlyOutput(ns("sex_plot"), height = "420px"),
                       DT::DTOutput(ns("sex_table"), fill = FALSE)),
      bslib::nav_panel("Events", shiny::uiOutput(ns("events_note")), DT::DTOutput(ns("events_table"), fill = FALSE)))))
}

explore_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nothing <- shiny::renderUI(if (is.null(result())) nothing_yet())
    tables_server("tables", result)

    time <- shiny::reactive(safely(qc_time_summary(shiny::req(result()))))
    # utf8_safe: text from the data is shown, so it must be valid UTF-8 (see 60_app.R).
    taxa <- shiny::reactive(safely(utf8_safe(qc_taxa_summary(shiny::req(result())))))
    emof <- shiny::reactive(safely(utf8_safe(qc_emof_summary(shiny::req(result())))))
    event_levels <- shiny::reactive(safely(qc_event_levels(shiny::req(result()))))

    # --- Time ---
    output$time_note <- shiny::renderUI({
      t <- time()
      if (failed(t)) return(error_alert(t, "The time overview"))
      if (is.null(t)) return(shiny::p(class = "text-muted mt-2", "No eventDate in this dataset."))
      shiny::p(class = "text-muted mt-2", sprintf(
        "Records (%s) per %s. Shaded: the time span in the metadata%s. Dashed: the middle 95 %% of the records (%s to %s).%s%s",
        t$table, t$unit, if (!nrow(t$coverage)) " (none given)" else "", format(t$band[1]), format(t$band[2]),
        if (t$undated > 0) sprintf(" %s records without a valid date.", format(t$undated, big.mark = ",")) else "",
        if (!is.na(t$outside_coverage) && t$outside_coverage > 0) {
          sprintf(" %s records lie outside the metadata time span.", format(t$outside_coverage, big.mark = ","))
        } else ""))
    })

    output$time_plot <- plotly::renderPlotly({
      t <- time()
      shiny::req(!failed(t), !is.null(t), nrow(t$counts) > 0)
      label <- format(t$counts$period, if (t$unit == "year") "%Y" else "%B %Y")
      # The metadata time span (shaded) and the middle 95 % (dashed lines) as shapes behind the bars.
      span <- lapply(seq_len(nrow(t$coverage)), function(i) {
        list(type = "rect", xref = "x", yref = "paper", x0 = as.character(t$coverage$begin[i]), x1 = as.character(t$coverage$end[i]),
             y0 = 0, y1 = 1, fillcolor = "rgba(31, 92, 153, 0.12)", line = list(width = 0), layer = "below")
      })
      band <- lapply(t$band, function(date) {
        list(type = "line", xref = "x", yref = "paper", x0 = as.character(date), x1 = as.character(date), y0 = 0, y1 = 1,
             line = list(color = "#D68910", dash = "dash", width = 2))
      })
      chart <- plotly::plot_ly(t$counts, x = ~period, y = ~records, type = "bar", marker = list(color = "#1F5C99"),
                               text = sprintf("%s: %s records", label, format(t$counts$records, big.mark = ",")),
                               hoverinfo = "text", textposition = "none")
      chart_layout(plotly::layout(chart, shapes = c(span, band)), x = "", y = "records")
    })

    # --- Taxa ---
    output$taxa_note <- shiny::renderUI({
      x <- taxa()
      if (failed(x)) return(error_alert(x, "The taxa overview"))
      if (!nrow(x)) return(shiny::p(class = "text-muted mt-2", "No occurrences in this dataset."))
      shiny::p(class = "text-muted mt-2", sprintf(
        "%s different taxa. The 20 with most records; orange: a taxonomy check found something. WoRMS details come from the stored lookups.",
        format(nrow(x), big.mark = ",")))
    })

    output$taxa_plot <- plotly::renderPlotly({
      x <- taxa()
      shiny::req(!failed(x), nrow(x) > 0)
      top <- utils::head(x, 20)
      top[, label := ifelse(is.na(scientificName), paste0("(no name) ", scientificNameID), scientificName)]
      top[, found := ifelse(is.na(issues), "no issues", "issues")]
      top[, hover := sprintf("<b>%s</b><br>%s records<br>%s%s%s<br>%s", htmltools::htmlEscape(label), format(records, big.mark = ","),
                             ifelse(is.na(rank), "", paste0(rank, ", ")), ifelse(is.na(status), "not looked up in WoRMS", status),
                             ifelse(is.na(accepted_name), "", paste0("<br>accepted: ", accepted_name)),
                             ifelse(is.na(issues), "no taxonomy issues", paste("issues:", issues)))]
      chart <- plotly::plot_ly(top, x = ~records, y = ~label, color = ~found, colors = c("no issues" = "#1F5C99", issues = "#D68910"),
                               type = "bar", orientation = "h", text = ~hover, hoverinfo = "text", textposition = "none")
      chart_layout(plotly::layout(chart, yaxis = list(categoryorder = "array", categoryarray = rev(top$label), title = ""),
                                  legend = list(orientation = "h", y = 1.08)), x = "records")
    })

    output$taxa_table <- DT::renderDT({
      x <- taxa()
      shiny::req(!failed(x), nrow(x) > 0)
      filter_table(x, 10, selection = "none", dropdowns = c("rank", "status", "habitat"))
    })

    # --- Measurements ---
    output$emof_note <- shiny::renderUI({
      x <- emof()
      if (failed(x)) return(error_alert(x, "The measurements overview"))
      if (is.null(x)) return(shiny::p(class = "text-muted mt-2", "No measurements (eMoF) in this dataset."))
      shiny::p(class = "text-muted mt-2", "One row per measurement type and unit: how many records, the range of the numbers,",
               "examples of text values, the NERC label of the type (from the stored lookups) and the issues found.")
    })

    output$emof_table <- DT::renderDT({
      x <- emof()
      shiny::req(!failed(x), !is.null(x))
      x <- copy(x)[, `:=`(min = signif(min, 4), median = signif(median, 4), max = signif(max, 4))]
      filter_table(x, 15, selection = "none", dropdowns = c("measurementUnit", "measurementUnitID"))
    })

    # --- Distributions and relationships: the numbers of the dataset ---
    variables <- shiny::reactive(safely(utf8_safe(qc_numeric_variables(shiny::req(result())))))

    shiny::observeEvent(variables(), {
      v <- variables()
      if (failed(v) || !nrow(v)) return()
      choices <- stats::setNames(v$id, sprintf("%s (%s values)", v$label, format(v$values, big.mark = ",")))
      shiny::updateSelectInput(session, "dist_variable", choices = choices)
      measured <- v[kind != "year"]
      measured_choices <- stats::setNames(measured$id, measured$label)
      guess <- function(pattern, fallback) {
        hit <- measured$id[grepl(pattern, measured$label, ignore.case = TRUE)]
        if (length(hit)) hit[1] else fallback
      }
      shiny::updateSelectInput(session, "rel_x", choices = measured_choices, selected = guess("length|lengte|size", measured$id[1]))
      shiny::updateSelectInput(session, "rel_y", choices = measured_choices,
                               selected = guess("weight|weigth|mass", measured$id[min(2, nrow(measured))]))
    })

    shiny::observeEvent(taxa(), {
      x <- taxa()
      if (failed(x) || !nrow(x)) return()
      taxon_names <- utils::head(stats::na.omit(x$scientificName), 500)
      for (input_id in c("dist_taxon", "rel_taxon")) {
        shiny::updateSelectizeInput(session, input_id, choices = c("(all taxa)" = "", taxon_names), server = TRUE)
      }
    })

    # The values of the chosen variable (and taxon), with their taxon (for the box plot per taxon).
    distribution_values <- shiny::reactive({
      shiny::req(input$dist_variable)
      safely({
        values <- qc_variable_values(result(), input$dist_variable, variables())
        if (length(input$dist_taxon) && nzchar(input$dist_taxon)) values <- values[taxon %chin% input$dist_taxon]
        values
      })
    })
    distribution <- shiny::reactive({
      v <- distribution_values()
      if (failed(v)) v else v$value
    })

    output$dist_note <- shiny::renderUI({
      v <- distribution()
      if (failed(v)) return(error_alert(v, "The distribution"))
      if (!length(v)) return(shiny::p(class = "text-muted mt-2", "No values for this choice."))
      q <- stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
      dropped <- if (isTRUE(input$dist_log)) sum(v <= 0) else 0
      shiny::p(class = "text-muted mt-2", sprintf("%s values: minimum %s, quartiles %s / %s / %s, maximum %s.%s",
        format(length(v), big.mark = ","), signif(q[1], 4), signif(q[2], 4), signif(q[3], 4), signif(q[4], 4), signif(q[5], 4),
        if (dropped > 0) sprintf(" %s values of 0 or less are left out on the log scale.", format(dropped, big.mark = ",")) else ""))
    })

    output$dist_plot <- plotly::renderPlotly({
      v <- distribution()
      shiny::req(!failed(v), length(v) > 0)
      if (identical(input$dist_chart, "box")) return(box_chart(distribution_values(), isTRUE(input$dist_log),
                                                               variables()$label[variables()$id == input$dist_variable]))
      on_log <- isTRUE(input$dist_log)
      if (on_log) v <- log10(v[v > 0])
      shiny::req(length(v) > 0)
      # Bin first (fast also for millions of values), then draw the bars.
      bins <- graphics::hist(v, breaks = input$dist_bins, plot = FALSE)
      shown <- function(edge) signif(if (on_log) 10^edge else edge, 4)
      bars <- data.table(mid = bins$mids, count = bins$counts,
                         hover = sprintf("%s to %s: %s records", shown(utils::head(bins$breaks, -1)), shown(bins$breaks[-1]),
                                         format(bins$counts, big.mark = ",")))
      chart <- plotly::plot_ly(bars, x = ~mid, y = ~count, type = "bar", marker = list(color = "#1F5C99"),
                               text = ~hover, hoverinfo = "text", textposition = "none")
      chart_layout(plotly::layout(chart, bargap = 0.05),
                   x = if (on_log) "log10(value)" else variables()$label[variables()$id == input$dist_variable], y = "records")
    })

    relationship <- shiny::reactive({
      shiny::req(input$rel_x, input$rel_y, input$rel_model)
      safely(utf8_safe(qc_relationship(result(), input$rel_x, input$rel_y, model = input$rel_model,
                                       threshold = if (is.numeric(input$rel_threshold)) input$rel_threshold else 3,
                                       taxon = input$rel_taxon, variables = variables())))
    })

    output$rel_note <- shiny::renderUI({
      rel <- relationship()
      if (failed(rel)) return(error_alert(rel, "The relationship"))
      if (is.null(rel)) return(shiny::p(class = "text-muted mt-2", "Fewer than 5 records have both values for this choice."))
      far <- rel$points[far == TRUE]
      shiny::p(class = "text-muted mt-2", sprintf(
        "%s records with both values. Fit: %s (R² %.2f). Orange: %s records (%s different value pairs) far from the curve.",
        format(nrow(rel$points), big.mark = ","), rel$formula, rel$r_squared, format(nrow(far), big.mark = ","),
        format(nrow(unique(far[, .(x, y)])), big.mark = ",")))
    })

    output$rel_plot <- plotly::renderPlotly({
      rel <- relationship()
      shiny::req(!failed(rel), !is.null(rel))
      # One point per different (x, y) pair, with one of its records and how many records have it.
      points <- rel$points[, .(owner = owner[1], taxon = taxon[1], fitted = fitted[1], records = .N), by = .(x, y, far)]
      normal <- points[far == FALSE]
      if (nrow(normal) > 20000) normal <- normal[sample.int(.N, 20000)]       # enough to see the cloud
      shown <- rbind(normal, points[far == TRUE])
      record_ids <- utf8_safe(record_pointer(result()$dataset, record_table(result()$dataset), shown$owner)$record_id)
      shown[, hover := sprintf("<b>%s</b>%s<br>x %s, y %s<br>curve: y %s%s", htmltools::htmlEscape(record_ids),
                               ifelse(is.na(taxon), "", paste0("<br>", htmltools::htmlEscape(taxon))), signif(x, 5), signif(y, 5), signif(fitted, 4),
                               ifelse(records > 1, sprintf("<br>%s records with these values", format(records, big.mark = ",")), ""))]
      chart <- plotly::plot_ly()
      chart <- plotly::add_trace(chart, data = shown[far == FALSE], x = ~x, y = ~y, type = "scattergl", mode = "markers",
                                 name = "close to the curve", marker = list(color = "rgba(31, 92, 153, 0.5)", size = 6),
                                 text = ~hover, hoverinfo = "text")
      chart <- plotly::add_trace(chart, data = shown[far == TRUE], x = ~x, y = ~y, type = "scattergl", mode = "markers",
                                 name = "far from the curve", marker = list(color = "#D68910", size = 9),
                                 text = ~hover, hoverinfo = "text")
      chart <- plotly::add_trace(chart, data = rel$curve, x = ~x, y = ~y, type = "scatter", mode = "lines", name = rel$formula,
                                 line = list(color = "#C0392B", width = 2), hoverinfo = "skip")
      chart_layout(plotly::layout(chart, legend = list(orientation = "h", y = 1.1)),
                   x = variables()$label[variables()$id == input$rel_x], y = variables()$label[variables()$id == input$rel_y])
    })

    output$rel_table <- DT::renderDT({
      rel <- relationship()
      shiny::req(!failed(rel), !is.null(rel))
      far <- rel$points[far == TRUE][order(-abs(residual))][seq_len(min(.N, 5000))]
      shiny::req(nrow(far) > 0)
      base <- record_table(result()$dataset)
      shown <- data.table(record_id = utf8_safe(record_pointer(result()$dataset, base, far$owner)$record_id), taxon = far$taxon,
                          x = far$x, y = far$y, `y from the curve` = signif(far$fitted, 4))
      filter_table(shown, 10, selection = "none", dropdowns = "taxon")
    })

    # --- Sexes ---
    sexes <- shiny::reactive(safely(utf8_safe(qc_sex_ratio(shiny::req(result())))))

    output$sex_note <- shiny::renderUI({
      x <- sexes()
      if (failed(x)) return(error_alert(x, "The sexes"))
      if (is.null(x)) return(shiny::p(class = "text-muted mt-2", "No sex in this dataset (no field sex, no measurement of sex or gender)."))
      shiny::p(class = "text-muted mt-2", "Males, females and other values (unknown, juvenile, ...) per taxon; the 20 taxa with most records.",
               "'share_female' is females / (females + males).")
    })

    output$sex_plot <- plotly::renderPlotly({
      x <- sexes()
      shiny::req(!failed(x), !is.null(x))
      top <- utils::head(x, 20)
      long <- melt(top[, .(taxon, female, male, other)], id.vars = "taxon", variable.name = "sex", value.name = "records")
      long[, hover := sprintf("<b>%s</b><br>%s: %s records", htmltools::htmlEscape(taxon), sex, format(records, big.mark = ","))]
      chart <- plotly::plot_ly(long, x = ~records, y = ~taxon, color = ~sex,
                               colors = c(female = "#C0392B", male = "#1F5C99", other = "#BDC3C7"),
                               type = "bar", orientation = "h", text = ~hover, hoverinfo = "text", textposition = "none")
      chart_layout(plotly::layout(chart, barmode = "stack", legend = list(orientation = "h", y = 1.08),
                                  yaxis = list(categoryorder = "array", categoryarray = rev(top$taxon), title = "")), x = "records")
    })

    output$sex_table <- DT::renderDT({
      x <- sexes()
      shiny::req(!failed(x), !is.null(x))
      filter_table(x, 10, selection = "none", dropdowns = character())
    })

    # --- Events ---
    output$events_note <- shiny::renderUI({
      x <- event_levels()
      if (failed(x)) return(error_alert(x, "The event overview"))
      if (is.null(x)) return(shiny::p(class = "text-muted mt-2", "No event table in this dataset."))
      shiny::p(class = "text-muted mt-2", "Level 1 are events without a parent (e.g. a cruise), level 2 their children",
               "(e.g. stations), and so on. 'Own date' and 'own position': events that have them themselves, not from a parent.")
    })

    output$events_table <- DT::renderDT({
      x <- event_levels()
      shiny::req(!failed(x), !is.null(x))
      plain_table(x, 20)
    })
  })
}
