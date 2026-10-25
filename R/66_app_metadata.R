# =============================================================================
# App screen 4: Metadata (EML), separate from the data checks. The FAIR
# letters, the metadata issues, and what the metadata says (so a person can
# see what is there and what is missing).
# =============================================================================

metadata_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

metadata_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$page <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(nothing_yet())
      if (identical(r$dataset$source$type, "table")) {
        return(alert("A simple table has no metadata (EML). The metadata checks apply to Darwin Core Archives.", "secondary"))
      }
      page <- safely(shiny::tagList(fair_section(r), metadata_issues_section(r), metadata_content_section(r)))
      if (failed(page)) error_alert(page, "The metadata") else page
    })
  })
}

fair_section <- function(r) {
  fair <- qc_fair(r)
  letter_names <- c(F = "Findable", A = "Accessible", I = "Interoperable", R = "Reusable")
  boxes <- lapply(seq_len(nrow(fair$summary)), function(i) {
    s <- fair$summary[i]
    bslib::value_box(letter_names[[s$letter]], sprintf("%d / %d", s$passed, s$counted), "checks passed",
                     theme = if (s$counted == 0) "secondary" else if (s$passed == s$counted) "success" else if (s$passed >= s$counted / 2) "warning" else "danger")
  })
  details <- fair$checks[, .(letter, `looks for` = title, status, issues)]
  shiny::tagList(
    shiny::h4(class = "mt-3", "FAIR"),
    shiny::p(class = "text-muted", "A quick indication, not a certification. Findable, Accessible and Reusable are about the",
             "metadata; Interoperable also counts the standard terms and vocabularies in the data."),
    do.call(bslib::layout_column_wrap, c(list(width = 1 / 4), boxes)),
    bslib::accordion(open = FALSE, class = "mt-2",
                     bslib::accordion_panel("Which checks count for which letter", plain_table(details, 40))))
}

metadata_issues_section <- function(r) {
  titles <- titles_of_checks(r)
  # Also the licence when it is a column of the data: the licence belongs to the metadata.
  issues <- metadata_problems(r)[, .(severity, problem = titles[check_id],
                                     where = ifelse(table == "eml", paste("eml.xml:", ifelse(is.na(field), "(the whole file)", field)),
                                                    sprintf("%s table, column %s", table_label(table), field)),
                                     value = shorten(value, 120), `what is wrong` = message, suggestion = shorten(suggestion, 120),
                                     records = ifelse(table == "eml", "", format(records, big.mark = ",")))]
  bslib::card(class = "mt-4",
              bslib::card_header(sprintf("Metadata issues: %d errors, %d warnings, %d info", sum(issues$severity == "error"),
                                         sum(issues$severity == "warning"), sum(issues$severity == "info"))),
              bslib::card_body(if (nrow(issues)) colour_severity(filter_table(issues, 15, selection = "none"))
                               else shiny::p(class = "mb-0", "No issues in the metadata.")))
}

# What the EML says, as plain text: what is there and what is missing.
metadata_content_section <- function(r) {
  m <- r$dataset$metadata
  if (is.null(m)) return(alert("This dataset has no metadata file (eml.xml).", "warning"))
  first <- function(x) {
    x <- unlist(x, use.names = FALSE)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    if (!length(x)) "(missing)" else as.character(x[1])
  }
  people <- function(wanted_role) {   # not 'role': inside p[...] that would mean the column
    p <- m$people
    if (is.null(p) || !nrow(p)) return("(missing)")
    chosen <- p[role == wanted_role]
    if (!nrow(chosen)) return("(missing)")
    names <- trimws(paste(ifelse(is.na(chosen$given_name), "", chosen$given_name), ifelse(is.na(chosen$sur_name), "", chosen$sur_name)))
    names <- ifelse(nzchar(names), names, chosen$organization)
    paste(unique(stats::na.omit(names)), collapse = "; ")
  }
  periods <- m$temporal
  coverage_time <- if (is.null(periods) || !NROW(periods)) "(missing)" else {
    paste(ifelse(!is.na(periods$single), periods$single, paste(periods$begin, "to", periods$end)), collapse = "; ")
  }
  boxes <- m$geographic
  coverage_area <- if (is.null(boxes) || !NROW(boxes)) "(missing)" else {
    sprintf("%s (%d area%s; first: %s to %s E, %s to %s N)", first(boxes$description), NROW(boxes), if (NROW(boxes) > 1) "s" else "",
            boxes$west[1], boxes$east[1], boxes$south[1], boxes$north[1])
  }
  taxa <- m$taxonomic$taxa
  coverage_taxa <- if (is.null(taxa) || !NROW(taxa)) first(m$taxonomic$description) else {
    sprintf("%d taxa listed; %s", NROW(taxa), shorten(first(m$taxonomic$description), 150))
  }
  says <- data.table(
    item = c("Title", "Abstract", "Licence", "Citation", "Contact", "Creator", "Metadata provider", "Keywords",
             "Time span", "Area", "Taxa", "Methods", "DOI / identifier"),
    value = c(first(m$title), shorten(first(m$abstract), 300), first(m$license$name), shorten(first(m$citation), 200),
              people("contact"), people("creator"), people("metadataProvider"),
              if (is.null(m$keywords) || !NROW(m$keywords)) "(missing)" else shorten(paste(unique(m$keywords$keyword), collapse = "; "), 200),
              coverage_time, coverage_area, coverage_taxa, shorten(first(m$methods), 200), first(c(m$doi, m$identifiers))))
  bslib::card(class = "mt-4 mb-4",
              bslib::card_header("What the metadata says"),
              bslib::card_body(plain_table(says, 20)))
}
