# =============================================================================
# Column mapping for simple tables (CSV / Excel that are not Darwin Core yet)
#
# For every column the tool proposes a Darwin Core term, and says how it found it:
#   exact     the column name is the term                      eventDate
#   capitals  same name, other capitals                        EventDate, eventdate
#   synonym   in the list inst/reference/term_synonyms.csv     Latitude, lat, species
#   similar   1 letter different from a Darwin Core term       decimalLatitud
#             (2 letters for names of 10+ characters; never DNA/MIxS terms)
#   none      no proposal: the column is not used
# 'similar' proposals deserve a look. Your own mapping always wins:
#   qc_check("data.csv", mapping = c(Date = "eventDate", Station = "locality", Day = NA))
# (NA or "" means: do not use this column.)
# =============================================================================

#' Propose Darwin Core terms for the columns of a table
#'
#' @param columns Column names (or a data frame, whose names are used).
#' @param mapping Optional own mapping that overrides the proposals: a named
#'   character vector `c(column = "term")`; `NA` or `""` leaves a column out.
#' @return A data.table: column, term, how (exact, capitals, synonym, similar,
#'   your mapping, none), note.
#' @export
#' @examples
#' qc_suggest_mapping(c("Date", "Latitude", "Longitude", "species", "Mesh_size"))
qc_suggest_mapping <- function(columns, mapping = NULL) {
  if (is.data.frame(columns)) columns <- names(columns)
  term_info <- term_table()
  terms <- term_info$term
  # 'similar' only proposes general terms; DNA (MIxS, GGBN, ...) terms need an exact name.
  general_terms <- term_info$term[term_info$namespace %in% c("dwc", "dcterms", "dc", "obis")]
  synonyms <- reference_table("term_synonyms.csv")

  proposals <- rbindlist(lapply(columns, function(column) {
    found <- function(term, how, note = "") data.table(column = column, term = term, how = how, note = note)
    if (column %in% terms) return(found(column, "exact"))
    same_letters <- match(tolower(column), tolower(terms))
    if (!is.na(same_letters)) return(found(terms[same_letters], "capitals"))
    synonym <- match(simple_name(column), simple_name(synonyms$wrong_name))
    if (!is.na(synonym)) return(found(synonyms$term[synonym], "synonym", synonyms$note[synonym]))
    if (nchar(column) >= 5) {
      # Short names: 1 letter different at most ('Mean_Pred' is not 'feat_pred'); longer names: 2.
      allowed <- if (nchar(column) < 10) 1 else 2
      distance <- utils::adist(tolower(column), tolower(general_terms))[1, ]
      if (min(distance) <= allowed) return(found(general_terms[which.min(distance)], "similar", "please check"))
    }
    found(NA_character_, "none")
  }))

  # False friends: 'Group', 'Member', 'Bed' are Darwin Core terms about rock
  # layers (geology). A column of a biological table with that name means
  # something else, so it is only used when its name is exactly the term.
  geology <- term_info$term[term_info$class_group %in% "GeologicalContext"]
  false_friend <- proposals$how %in% c("capitals", "similar") & proposals$term %in% geology
  proposals[false_friend, `:=`(note = sprintf("the Darwin Core term '%s' is about rock layers (geology)", term),
                               term = NA_character_, how = "none")]

  # Your own mapping wins.
  if (length(mapping)) {
    unknown <- setdiff(names(mapping), columns)
    if (length(unknown)) stop("mapping names column(s) that are not in the table: ", paste(unknown, collapse = ", "), call. = FALSE)
    own <- match(names(mapping), proposals$column)
    proposals$term[own] <- ifelse(is.na(mapping) | mapping == "", NA_character_, unname(mapping))
    proposals$how[own] <- "your mapping"
    proposals$note[own] <- ""
  }

  # Two columns proposed for the same term: keep the most certain one (the
  # first, when equally certain). By position: a table can repeat a column name.
  certainty <- c("your mapping" = 1, exact = 2, capitals = 3, synonym = 4, similar = 5)
  proposals[, position := .I]
  ranked <- proposals[!is.na(term)][order(certainty[how], position)]
  kept <- ranked[!duplicated(term)]
  for (i in ranked[duplicated(term), position]) {
    proposals$note[i] <- sprintf("'%s' already goes to the column '%s' (line %d of this list)", proposals$term[i],
                                 kept$column[kept$term == proposals$term[i]], kept$position[kept$term == proposals$term[i]])
    proposals$term[i] <- NA_character_
    proposals$how[i] <- "none"
  }
  proposals[, position := NULL]
  proposals[is.na(term), how := ifelse(how == "your mapping", "your mapping", "none")]
  proposals
}

# "Station_nr", "station nr", "STATION-NR" -> "stationnr"
simple_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))
