# =============================================================================
# The figure "How the QC Tool is built" for the manual: inst/manual/architecture.png
# Run from the qctool folder after the structure of the tool changes:
#   Rscript dev/make_architecture_figure.R
# Orange: what the data management team edits without programming.
# =============================================================================

library(grid)

colours <- list(code = "#DCE8F5", edit = "#FCE5C8", input = "#EEEEEE", line = "#1F5C99", text = "#1D1F21")

# The number of checks, from the rules workbook.
n_checks <- nrow(openxlsx2::read_xlsx("inst/rules/qc_rules.xlsx", sheet = "Checks"))

# A box with a bold title and lines of text, centred on (x, y).
box <- function(x, y, w, title, lines, fill) {
  h <- 0.055 + 0.035 * length(lines)
  grid.roundrect(x, y, w, h, r = unit(0.012, "snpc"), gp = gpar(fill = fill, col = colours$line, lwd = 1.3))
  grid.text(title, x, y + h / 2 - 0.03, gp = gpar(fontface = "bold", fontsize = 12, col = colours$text))
  for (i in seq_along(lines)) {
    grid.text(lines[i], x, y + h / 2 - 0.03 - 0.035 * i, gp = gpar(fontsize = 9.5, col = colours$text))
  }
  invisible(list(x = x, y = y, w = w, h = h))
}

edge <- function(b, side) {
  switch(side, left = c(b$x - b$w / 2, b$y), right = c(b$x + b$w / 2, b$y),
         top = c(b$x, b$y + b$h / 2), bottom = c(b$x, b$y - b$h / 2))
}

link <- function(from, to, both = FALSE) {
  grid.segments(from[1], from[2], to[1], to[2],
                arrow = arrow(angle = 22, length = unit(0.09, "inches"), type = "closed", ends = if (both) "both" else "last"),
                gp = gpar(col = colours$line, fill = colours$line, lwd = 1.4))
}

png("inst/manual/architecture.png", width = 12, height = 7.6, units = "in", res = 200)
grid.newpage()
grid.rect(gp = gpar(fill = "white", col = NA))
grid.text("How the QC Tool is built", 0.03, 0.965, just = "left", gp = gpar(fontsize = 18, fontface = "bold", col = colours$line))
grid.text("Blue: R code (folder R/). Orange: edited by the data management team, no programming. Grey: what goes in and comes out.",
          0.03, 0.925, just = "left", gp = gpar(fontsize = 10, col = "#555555"))

inputs   <- box(0.10, 0.56, 0.17, "Input", c("Darwin Core Archive (zip)", "IPT / GBIF / OBIS address", "table: CSV, TXT, Excel"), colours$input)
readers  <- box(0.30, 0.56, 0.16, "Readers", c("R/10_ ... R/14_", "meta.xml, eml.xml,", "column mapping (tables)"), colours$code)
dataset  <- box(0.50, 0.56, 0.17, "Dataset", c("event, occurrence, eMoF, DNA", "+ the metadata (EML)", "R/20: links, inheritance"), colours$code)
checks   <- box(0.72, 0.56, 0.19, "Engine and checks", c("qc_check(): R/01_, R/02_", sprintf("%d checks: check_<id>()", n_checks), "R/checks_01_ ... _15_"), colours$code)
rules    <- box(0.62, 0.80, 0.21, "Rules workbook", c("inst/rules/qc_rules.xlsx", "severity and settings per check,", "required fields, field lengths"), colours$edit)
lists    <- box(0.87, 0.80, 0.21, "Reference lists", c("inst/reference/*.csv", "vocabularies, synonyms,", "placeholders, NERC terms"), colours$edit)
lookups  <- box(0.915, 0.50, 0.15, "Online lookups", c("R/30_ ... R/33_", "WoRMS, NERC, OBIS", "answers kept 30 days"), colours$code)
result   <- box(0.72, 0.30, 0.21, "Result", c("problems + the records they affect", "R/03_, R/04_", "qc_issues(), qc_record()"), colours$code)
reports  <- box(0.36, 0.08, 0.20, "Reports", c("Excel, CSV, JSON", "R/50_, R/51_"), colours$input)
app      <- box(0.60, 0.08, 0.20, "App: qc_app()", c("screens 1-7 and Help", "R/60_ ... R/69_"), colours$input)
summary  <- box(0.84, 0.08, 0.22, "Summaries and maps in R", c("essentials, FAIR, explore, tables", "qc_map(): R/45_ ... R/49_"), colours$input)

top_of_checks <- checks$y + checks$h / 2
link(edge(inputs, "right"), edge(readers, "left"))
link(edge(readers, "right"), edge(dataset, "left"))
link(edge(dataset, "right"), edge(checks, "left"))
link(c(0.66, rules$y - rules$h / 2), c(0.66, top_of_checks))     # both arrows land on the engine box
link(c(0.79, lists$y - lists$h / 2), c(0.79, top_of_checks))
link(c(checks$x + checks$w / 2, lookups$y), edge(lookups, "left"), both = TRUE)
link(edge(checks, "bottom"), edge(result, "top"))
for (target in list(reports, app, summary)) link(edge(result, "bottom"), edge(target, "top"))
dev.off()
cat("Written: inst/manual/architecture.png\n")
