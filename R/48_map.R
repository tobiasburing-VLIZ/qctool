# =============================================================================
# A map of the positions with problems, without the app
#
#   qc_map(result)                    interactive map (RStudio Viewer / browser)
#   qc_map(result, "map.html")        the same, saved as a web page
#   qc_map(result, "map.png")         a simple picture (also .pdf): coastline
#                                     from the 'maps' package, no internet needed
#
# The points come from qc_map_points() (45_summaries.R), like the app's map.
# =============================================================================

#' A map of the positions with problems
#'
#' Positions with an issue about latitude, longitude or depth (on land, swapped,
#' 0/0, far outside the sampling area, deeper than the sea floor, ...) in
#' colour: errors red, warnings orange, info blue. Other positions in grey.
#'
#' @param result A result of [qc_check()].
#' @param file Optional. Ending in ".html": the interactive map is saved as a
#'   web page (with a folder of its scripts next to it). Ending in ".png" or
#'   ".pdf": a simple picture with the coastline of the 'maps' package.
#' @param severity Which issues to show (default all three).
#' @param checks Optional check ids to show (default all with positions).
#' @param others Also show the positions without problems (grey).
#' @return The interactive map (printing it shows it); for a picture the file
#'   path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' result <- qc_check("dwca-biofun_2009-v1.9.zip")
#' qc_map(result)                                  # in the RStudio Viewer
#' qc_map(result, "biofun_map.png")                # a picture for a report
#' qc_map(result, severity = "error")              # errors only
#' }
qc_map <- function(result, file = NULL, severity = c("error", "warning", "info"), checks = NULL, others = TRUE) {
  points <- qc_map_points(result)
  # (Not problems[severity %in% severity]: inside [...] 'severity' would mean the column.)
  wanted_severity <- severity
  problems <- points$problems[points$problems$severity %in% wanted_severity]
  if (length(checks)) problems <- problems[problems$check_id %in% checks]
  background <- if (others) points$background else points$background[0]
  titles <- stats::setNames(result$checks$title, result$checks$check_id)
  boxes <- eml_boxes(result$dataset, required = FALSE)
  title <- if (is.na(result$run$dataset_title)) result$run$source_name else result$run$dataset_title

  if (!is.null(file) && grepl("\\.(png|pdf)$", file, ignore.case = TRUE)) {
    return(map_picture(problems, background, boxes, titles, title, file))
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The interactive map needs the package leaflet (install.packages(\"leaflet\")), ",
         "or save a picture: qc_map(result, \"map.png\").", call. = FALSE)
  }
  map <- leaflet::addTiles(leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)))
  if (nrow(background)) {
    map <- leaflet::addCircleMarkers(map, lng = background$lon, lat = background$lat, radius = 3, stroke = FALSE,
                                     fillColor = "#7F8C8D", fillOpacity = 0.5)
  }
  if (nrow(boxes)) {
    map <- leaflet::addRectangles(map, lng1 = boxes$west, lat1 = boxes$south, lng2 = boxes$east, lat2 = boxes$north,
                                  fill = FALSE, color = "#1F5C99", weight = 2, dashArray = "6", label = "Area in the metadata")
  }
  if (nrow(problems)) {
    popup <- sprintf("<b>%s</b><br>%s<br>%s<br>Record: %s (%s)", htmltools::htmlEscape(titles[problems$check_id]),
                     htmltools::htmlEscape(problems$message), htmltools::htmlEscape(shorten(problems$value, 120)),
                     htmltools::htmlEscape(problems$record_id), htmltools::htmlEscape(problems$table))
    map <- leaflet::addCircleMarkers(map, lng = problems$lon, lat = problems$lat, radius = 6, weight = 1, color = "#FFFFFF",
                                     fillColor = unname(severity_colours[problems$severity]), fillOpacity = 0.9, popup = popup,
                                     label = sprintf("%s: %s", titles[problems$check_id], problems$record_id))
    map <- leaflet::addLegend(map, "bottomright", colors = unname(severity_colours), labels = names(severity_colours),
                              title = "Problems", opacity = 0.9)
  }
  map <- leaflet::addControl(map, htmltools::tags$div(htmltools::tags$b(title), htmltools::tags$br(),
                                                      sprintf("%s positions with a problem%s", format(nrow(problems), big.mark = ","),
                                                              if (points$problems_total > nrow(points$problems)) " (a sample)" else "")),
                             position = "topright")
  if (!is.null(file)) {
    htmlwidgets::saveWidget(map, file, selfcontained = FALSE, libdir = paste0(tools::file_path_sans_ext(basename(file)), "_files"),
                            title = paste("Map:", title))
    message("Saved: ", file)
  }
  map
}

# A simple picture: coastline, grey positions, coloured problems, legend.
map_picture <- function(problems, background, boxes, titles, title, file) {
  if (grepl("\\.png$", file, ignore.case = TRUE)) grDevices::png(file, width = 2000, height = 1400, res = 200)
  else grDevices::pdf(file, width = 10, height = 7)
  on.exit(grDevices::dev.off())

  all_lon <- c(problems$lon, background$lon)
  all_lat <- c(problems$lat, background$lat)
  if (!length(all_lon)) { all_lon <- c(-180, 180); all_lat <- c(-80, 80) }
  margin <- function(range) diff(range) * 0.1 + 1
  xlim <- range(all_lon) + c(-1, 1) * margin(range(all_lon))
  ylim <- pmin(pmax(range(all_lat) + c(-1, 1) * margin(range(all_lat)), -90), 90)

  graphics::par(mar = c(3, 3, 4, 1))
  if (requireNamespace("maps", quietly = TRUE)) {
    maps::map("world", xlim = xlim, ylim = ylim, fill = TRUE, col = "grey92", border = "grey65", mar = c(3, 3, 4, 1))
    maps::map.axes(cex.axis = 0.8)
  } else {
    graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", asp = 1)
  }
  if (nrow(background)) graphics::points(background$lon, background$lat, pch = 16, cex = 0.4, col = "grey55")
  if (nrow(boxes)) graphics::rect(boxes$west, boxes$south, boxes$east, boxes$north, border = "#1F5C99", lty = 2, lwd = 1.5)
  if (nrow(problems)) {
    ordered <- problems[order(severity_rank[problems$severity])]          # errors drawn last, on top
    graphics::points(ordered$lon, ordered$lat, pch = 21, cex = 1.1, col = "white", bg = severity_colours[ordered$severity])
  }
  graphics::title(main = title, cex.main = 1, line = 2.2)
  counts <- table(factor(problems$severity, names(severity_colours)))
  graphics::mtext(sprintf("%s positions with a problem (%s errors, %s warnings, %s info); grey: the other positions; dashed: the area in the metadata",
                          format(nrow(problems), big.mark = ","), counts[["error"]], counts[["warning"]], counts[["info"]]),
                  side = 3, line = 0.8, cex = 0.75)
  graphics::legend("bottomleft", legend = c("error", "warning", "info"), pt.bg = severity_colours, pch = 21, col = "white",
                   pt.cex = 1.6, bg = "white", cex = 0.8, title = "Problems")
  message("Saved: ", file)
  invisible(file)
}
