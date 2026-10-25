# =============================================================================
# Saving a result as an Excel workbook
#
# Sheets, from overview to detail (docs/design.md, section 4.7):
#   Summary            the verdict, dataset, records per table, counts
#   Checks             one row per check: status, issues, records affected
#   Issues grouped     one row per identical problem, with the number of records
#   Issues             issues with their record ID and file line, most important
#                      first.
#   Both issue sheets show at most max_issue_rows rows (default 100,000), the
#   most important first: writing Excel takes about 75 seconds per million rows,
#   nobody scrolls through a million rows, and Excel stops at 1,048,575. When
#   rows are left out the Summary says so; the complete lists are always in
#   issues.csv and issues_grouped.csv (qc_export_csv).
#   About the checks   what each check does and why, and how GBIF/OBIS differ
# =============================================================================

excel_max_rows <- 1048575L   # Excel's limit per sheet, minus the header row

#' Save a QC result as an Excel workbook
#'
#' @param result A result of [qc_check()].
#' @param file Path of the .xlsx file to write.
#' @param max_issue_rows Most rows written to the sheets 'Issues grouped' and
#'   'Issues', most important first (default 100,000; at most 1,048,575, Excel's
#'   limit). The complete lists are written by [qc_export_csv()].
#' @return The file path, invisibly.
#' @export
qc_export_xlsx <- function(result, file, max_issue_rows = 100000) {
  max_issue_rows <- min(max_issue_rows, excel_max_rows)
  total_issues <- sum(severity_counts(result))
  left_out <- c(issues = max(0, total_issues - max_issue_rows),
                problems = max(0, nrow(result$issues_grouped) - max_issue_rows))

  wb <- openxlsx2::wb_workbook()
  wb$add_dxfs_style(name = "error",   bg_fill = openxlsx2::wb_color("FFC7CE"), font_color = openxlsx2::wb_color("9C0006"))
  wb$add_dxfs_style(name = "warning", bg_fill = openxlsx2::wb_color("FFEB9C"), font_color = openxlsx2::wb_color("9C5700"))
  wb$add_dxfs_style(name = "info",    bg_fill = openxlsx2::wb_color("DDEBF7"), font_color = openxlsx2::wb_color("1F4E78"))
  wb$add_dxfs_style(name = "passed",  bg_fill = openxlsx2::wb_color("C6EFCE"), font_color = openxlsx2::wb_color("006100"))
  wb$add_dxfs_style(name = "could_not_run", bg_fill = openxlsx2::wb_color("EDEDED"), font_color = openxlsx2::wb_color("7F6000"))

  wb <- add_summary_sheet(wb, result, left_out)

  checks <- result$checks[, intersect(c("check_id", "group", "title", "severity", "status", "issues", "records_affected",
                                        "percent_affected", "reason", "duration_s"), names(result$checks)), with = FALSE]
  wb <- add_table_sheet(wb, "Checks", checks)
  wb <- colour_column(wb, "Checks", checks, "severity")
  wb <- colour_column(wb, "Checks", checks, "status")

  grouped <- utils::head(result$issues_grouped, max_issue_rows)
  wb <- add_table_sheet(wb, "Issues grouped", grouped)
  wb <- colour_column(wb, "Issues grouped", grouped, "severity")

  shown <- qc_issues(result, limit = max_issue_rows)[, table_row := NULL]   # 'line' is what people look for
  wb <- add_table_sheet(wb, "Issues", shown)
  wb <- colour_column(wb, "Issues", shown, "severity")

  about <- result$checks[, intersect(c("check_id", "group", "title", "severity", "what", "why", "other_standards"),
                                     names(result$checks)), with = FALSE]
  wb <- add_table_sheet(wb, "About the checks", about[order(group, check_id)])

  wb$save(file, overwrite = TRUE)
  invisible(file)
}

add_summary_sheet <- function(wb, result, left_out = c(issues = 0, problems = 0)) {
  run <- result$run
  counts <- severity_counts(result)
  judged <- verdict(result)   # R/04_result.R

  about_run <- data.table(
    item = c("Dataset", "Source", "Archive downloaded from", "IPT resource", "Checksum (MD5)", "Checked at",
             "Duration (s)", "Tool", "Rules workbook", rep("Note", length(run$reading_notes))),
    value = c(run$dataset_title, run$source, run$archive_url, run$ipt_url, run$checksum_md5, run$checked_at,
              as.character(run$duration_s), paste(run$tool, run$tool_version), run$rules_file, run$reading_notes))
  about_run <- about_run[!is.na(value)]
  by_status <- result$checks[, .(checks = .N), by = status][order(match(status, status_levels))]
  by_severity <- result$issues_grouped[, .(issues = sum(pmax(records, 1L)), problems = .N, checks = uniqueN(check_id)),
                                       by = severity][order(-severity_rank[severity])]

  sheet <- "Summary"
  wb$add_worksheet(sheet)
  wb$add_data(sheet, "QC Tool (Beta) - QC report", start_row = 1)
  wb$add_font(sheet, dims = "A1", bold = TRUE, size = 14)
  wb$add_data(sheet, paste0(toupper(judged$title), ": ", judged$text), start_row = 2)
  wb$add_font(sheet, dims = "A2", bold = TRUE, size = 12,
              color = openxlsx2::wb_color(c(errors = "9C0006", incomplete = "9C5700", warnings = "9C5700", clean = "006100")[[judged$state]]))
  shown_text <- function(sheet_name, left, total, what) {
    if (left <= 0) return(NULL)
    sprintf("The sheet '%s' shows the %s most important of %s %s.", sheet_name,
            format(total - left, big.mark = ","), format(total, big.mark = ","), what)
  }
  notes <- c(shown_text("Issues grouped", left_out[["problems"]], nrow(result$issues_grouped), "problems"),
             shown_text("Issues", left_out[["issues"]], sum(counts), "issues"))
  if (length(notes)) {
    note <- paste(c(notes, "The complete lists are in issues_grouped.csv and issues.csv (qc_export_csv)."), collapse = " ")
    wb$add_data(sheet, note, start_row = 3)
    wb$add_font(sheet, dims = "A3", italic = TRUE, color = openxlsx2::wb_color("9C5700"))
  }

  blocks <- list(list("About this check", about_run), list("Records per table", result$tables))
  if (nrow(judged$not_run)) {
    blocks <- c(list(list("Checks that did not run: their results are missing from this report",
                          judged$not_run[, .(check_id, title, severity, status, reason)])), blocks)
  }
  essentials <- tryCatch(qc_essentials(result), error = function(e) NULL)
  if (!is.null(essentials)) {
    blocks <- c(blocks, list(list("Essentials: occurrences without errors in these fields",
                                  essentials[, .(essential, fields, ok_percent, ok, occurrences, with_warnings, note)])))
  }
  fair <- tryCatch(qc_fair(result), error = function(e) NULL)
  if (!is.null(fair)) {
    blocks <- c(blocks, list(list(paste("FAIR metadata (a quick indication):", paste(fair$summary$text, collapse = " | ")),
                                  fair$checks)))
  }
  if (!is.null(result$dataset$mapping)) {
    mapping <- result$dataset$mapping[, .(column, term = ifelse(is.na(term), "(not used)", term), how, note)]
    blocks <- c(blocks, list(list("Column mapping (table -> Darwin Core)", mapping)))
  }
  blocks <- c(blocks, list(list("Checks by status", by_status), list("Issues by severity", by_severity)))
  if (!is.null(result$lookups) && nrow(result$lookups)) {
    lookups <- result$lookups[, .(service = lookup_labels[service], what, from_cache, from_internet)]
    blocks <- c(blocks, list(list(if (isFALSE(run$online)) "Online lookups (offline run: cache only)" else "Online lookups", lookups)))
  }

  row <- 4
  for (block in blocks) {
    wb$add_data(sheet, block[[1]], start_row = row)
    wb$add_font(sheet, dims = openxlsx2::wb_dims(rows = row, cols = 1), bold = TRUE)
    wb$add_data(sheet, block[[2]], start_row = row + 1)
    wb$add_font(sheet, dims = openxlsx2::wb_dims(rows = row + 1, cols = seq_len(ncol(block[[2]]))), bold = TRUE)
    row <- row + nrow(block[[2]]) + 3
  }
  wb$set_col_widths(sheet, cols = 1:6, widths = c(22, 70, 12, 12, 12, 12))
  wb
}

add_table_sheet <- function(wb, sheet, data) {
  wb$add_worksheet(sheet)
  if (!nrow(data)) {
    wb$add_data(sheet, "Nothing to report.")
    return(wb)
  }
  wb$add_data_table(sheet, data, table_style = "TableStyleLight1", with_filter = TRUE)
  wb$freeze_pane(sheet, first_active_row = 2)
  widths <- pmin(60, pmax(10, vapply(names(data), function(column) {
    sample <- utils::head(as.character(data[[column]]), 200)
    max(nchar(column), suppressWarnings(max(nchar(sample, type = "chars", allowNA = TRUE), na.rm = TRUE)), 0, na.rm = TRUE)
  }, numeric(1)) + 2))
  wb$set_col_widths(sheet, cols = seq_along(widths), widths = widths)
  wb
}

# Colour cells of a severity or status column with conditional formatting.
colour_column <- function(wb, sheet, data, column) {
  if (!nrow(data) || !column %in% names(data)) return(wb)
  dims <- openxlsx2::wb_dims(rows = seq_len(nrow(data)) + 1, cols = match(column, names(data)))
  colours <- if (column == "severity") {
    list(error = "error", warning = "warning", info = "info")
  } else {
    list("issues found" = "warning", "passed" = "passed", "check error" = "could_not_run",
         "not performed" = "could_not_run", "skipped" = "could_not_run")
  }
  for (text in names(colours)) {
    wb$add_conditional_formatting(sheet, dims = dims, type = "containsText", rule = text, style = colours[[text]])
  }
  wb
}
