# =============================================================================
# App screen 5: Map. Positions with a problem (on land, outliers, swapped,
# outside the metadata area, depth below the sea floor ...) in colour, all
# other positions in grey, and the area given in the metadata. For a person to
# judge: a point on land can be right (estuary, river, lagoon).
# Optional: tracks, the positions in date order per cruise, animal, vessel ...
# (qc_tracks() in 47_explore_more.R), with a dot at the start of each.
# =============================================================================

map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    class = "mt-3",
    sidebar = bslib::sidebar(
      width = 280,
      shiny::selectizeInput(ns("checks"), "Problems to show", choices = NULL, multiple = TRUE),
      shiny::checkboxInput(ns("background"), "All other positions (grey)", value = TRUE),
      shiny::checkboxInput(ns("eml_area"), "Area given in the metadata", value = TRUE),
      shiny::checkboxInput(ns("tracks"), "Tracks: positions in date order", value = FALSE),
      shiny::conditionalPanel(sprintf("input['%s']", ns("tracks")),
                              shiny::selectInput(ns("track_by"), "One track per", choices = NULL)),
      shiny::uiOutput(ns("note"))),
    shiny::uiOutput(ns("nothing")),
    bslib::card(full_screen = TRUE, leaflet::leafletOutput(ns("map"), height = "70vh"))
  )
}

map_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$nothing <- shiny::renderUI(if (is.null(result())) nothing_yet())

    points <- shiny::reactive(safely(utf8_safe(qc_map_points(shiny::req(result())))))

    shiny::observeEvent(points(), {
      if (failed(points())) return()
      problems <- points()$problems
      checks <- if (nrow(problems)) sort(unique(problems$check_id)) else character()
      titles <- titles_of_checks(result())
      shiny::updateSelectizeInput(session, "checks", choices = stats::setNames(checks, titles[checks]), selected = checks)
    })

    shiny::observeEvent(result(), {
      choices <- safely(utf8_safe(track_groupings(result())))
      if (!failed(choices)) shiny::updateSelectInput(session, "track_by", choices = choices)
    })

    tracks <- shiny::reactive({
      shiny::req(isTRUE(input$tracks), input$track_by)
      safely(utf8_safe(qc_tracks(result(), by = input$track_by)))
    })

    output$note <- shiny::renderUI({
      p <- points()
      if (failed(p)) return(error_alert(p, "The map"))
      shown_of <- function(shown, total) {
        if (shown < total) sprintf("%s of %s (a sample)", format(shown, big.mark = ","), format(total, big.mark = ","))
        else format(total, big.mark = ",")
      }
      shiny::helpText(sprintf("Records with a position problem: %s (errors first). Other positions: %s. Click a point for details.",
                              shown_of(nrow(p$problems), p$problems_total), shown_of(nrow(p$background), p$background_total)))
    })

    output$map <- leaflet::renderLeaflet({
      p <- points()
      map <- leaflet::addTiles(leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)))
      if (failed(p)) return(map)
      if (isTRUE(input$background) && nrow(p$background)) {
        map <- leaflet::addCircleMarkers(map, lng = p$background$lon, lat = p$background$lat, radius = 3, stroke = FALSE,
                                         fillColor = "#7F8C8D", fillOpacity = 0.5, group = "All positions")
      }
      if (isTRUE(input$eml_area)) {
        boxes <- eml_boxes(result()$dataset, required = FALSE)
        if (nrow(boxes)) {
          map <- leaflet::addRectangles(map, lng1 = boxes$west, lat1 = boxes$south, lng2 = boxes$east, lat2 = boxes$north,
                                        fill = FALSE, color = "#1F5C99", weight = 2, dashArray = "6", label = "Area in the metadata")
        }
      }
      if (isTRUE(input$tracks)) {
        t <- tracks()
        if (!failed(t) && !is.null(t) && nrow(t)) {
          track_names <- unique(t$track)
          colours <- stats::setNames(grDevices::hcl.colors(length(track_names), "Dark 3"), track_names)
          for (name in track_names) {
            one <- t[track == name]
            map <- leaflet::addPolylines(map, lng = one$lon, lat = one$lat, color = colours[[name]], weight = 2, opacity = 0.8,
                                         label = sprintf("%s: %s to %s (%d positions)", name, min(one$date), max(one$date), nrow(one)))
          }
          starts <- t[, .SD[1], by = track]
          map <- leaflet::addCircleMarkers(map, lng = starts$lon, lat = starts$lat, radius = 4, stroke = FALSE, fillOpacity = 1,
                                           fillColor = unname(colours[starts$track]), label = sprintf("start of %s, %s", starts$track, starts$date))
        }
      }
      shown <- p$problems[check_id %chin% input$checks]
      if (nrow(shown)) {
        titles <- titles_of_checks(result())
        popup <- sprintf("<b>%s</b><br>%s<br>%s<br>Record: %s (%s)", htmltools::htmlEscape(titles[shown$check_id]),
                         htmltools::htmlEscape(shown$message), htmltools::htmlEscape(shorten(shown$value, 120)),
                         htmltools::htmlEscape(shown$record_id), htmltools::htmlEscape(shown$table))
        map <- leaflet::addCircleMarkers(map, lng = shown$lon, lat = shown$lat, radius = 6, weight = 1, color = "#FFFFFF",
                                         fillColor = unname(severity_colours[shown$severity]), fillOpacity = 0.9, popup = popup,
                                         label = sprintf("%s: %s", titles[shown$check_id], shown$record_id))   # on hover
        map <- leaflet::addLegend(map, "bottomright", colors = unname(severity_colours), labels = names(severity_colours),
                                  title = "Problems", opacity = 0.9)
      }
      map
    })
  })
}
