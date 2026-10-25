# =============================================================================
# The manual as a Word document, for reading and sharing: docs/QC_Tool_manual.docx
#
#   Part 1  Overview          README.md
#   Part 2  Manual            inst/manual/manual.md
#   Appendix  The checks      inst/manual/checks.md (run dev/write_check_catalogue.R first)
#
# Run from the qctool folder after changing one of these files:
#   Rscript dev/make_word_manual.R
#
# Needs pandoc (comes with RStudio). The look (fonts, colours, tables, page
# numbers) is in dev/word_reference.docx: open it in Word and change its styles
# to change the look of the manual.
# =============================================================================

pandoc <- Sys.which("pandoc")
if (!nzchar(pandoc) && requireNamespace("rmarkdown", quietly = TRUE)) pandoc <- rmarkdown::pandoc_exec()
if (!nzchar(pandoc)) stop("pandoc not found. It comes with RStudio; or install it from https://pandoc.org")

read_text <- function(path) readLines(path, encoding = "UTF-8", warn = FALSE)

# Leave out a file's own title (the first line) and the lines '---' between sections:
# in Word each part starts on a new page and the headings do the rest.
body_of <- function(lines) {
  lines <- lines[-1]
  lines[lines != "---"]
}

readme <- body_of(read_text("README.md"))
manual <- read_text("inst/manual/manual.md")
# The manual's own list of contents: Word has a real one.
contents_start <- which(manual == "**Contents**")
first_section <- which(grepl("^## 1\\. ", manual))[1]
if (length(contents_start)) manual <- manual[-(contents_start:(first_section - 1))]
manual <- body_of(manual)
checks <- body_of(read_text("inst/manual/checks.md"))

version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
document <- c(
  "---",
  "title: \"QC Tool (Beta)\"",
  "subtitle: \"Quality control of Darwin Core datasets for EMODnet Biology / EurOBIS: overview and manual\"",
  sprintf("author: \"EMODnet Biology data management team, VLIZ - version %s\"", version),
  sprintf("date: \"%s\"", format(Sys.Date(), "%d %B %Y")),
  "---",
  "",
  "# Part 1: Overview", "", readme, "",
  "# Part 2: Manual", "", manual, "",
  "# Appendix: the checks", "", checks)

dir.create("docs", showWarnings = FALSE)
combined <- tempfile(fileext = ".md")
writeLines(enc2utf8(document), combined, useBytes = TRUE)
output <- file.path("docs", "QC_Tool_manual.docx")
status <- system2(pandoc, c(shQuote(combined), "-f", "gfm+yaml_metadata_block", "-o", shQuote(output),
                            "--reference-doc", shQuote("dev/word_reference.docx"),
                            "--resource-path", shQuote("inst/manual"),   # the pictures of the manual
                            "--toc", "--toc-depth=2", "--metadata", shQuote("toc-title=Contents")))
if (status != 0) stop("pandoc failed")
message("Written: ", output, " (Word updates the page numbers of the contents when you open it, or with F9)")
