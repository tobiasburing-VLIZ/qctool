# =============================================================================
# The engine: puts the checks in order and runs them
#
# Rules decided in the design (docs/design.md, section 4.4):
#   - Never stop halfway. Every check ends with exactly one status.
#   - Dependencies decide the ORDER. A check is only skipped when a check it
#     depends on could not run at all; issues found by that check do not stop it.
#   - Severity comes from the rules workbook, not from the check. A check may
#     lower the severity of single issues (severity_hint), never raise it.
#   - No limit on the number of issues.
# =============================================================================

# Every check ends with one of these statuses (listed in reports in this order).
status_levels <- c(
  "issues found",        # the check ran and found problems
  "check error",         # the check itself failed (a bug or unexpected data); others continue
  "not performed",       # needed something that was not available (e.g. internet)
  "skipped",             # a check it depends on could not run
  "not applicable",      # the data has nothing to check (e.g. no event table)
  "passed",              # the check ran and found nothing
  "not yet implemented", # in the rules workbook, but not built yet
  "switched off"         # severity 'off' in the rules workbook
)

# A check is skipped when one of its dependencies ended with one of these.
# 'not applicable' does not block: it only means that check had nothing to
# look at (e.g. no event table); the dependent check decides for itself.
blocking_statuses <- c("check error", "not performed", "skipped")

severity_rank <- c(info = 1, warning = 2, error = 3)

# --- Planning ----------------------------------------------------------------------

# The checks to run, in an order where every check comes after its dependencies.
plan_checks <- function(rules, only = NULL) {
  checks <- copy(rules$checks)
  checks[, added_as_dependency := FALSE]

  if (!is.null(only)) {
    unknown <- setdiff(only, checks$check_id)
    if (length(unknown)) stop("Unknown check id(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    needed <- with_dependencies(only, checks)
    checks <- checks[check_id %in% needed]
    checks[!check_id %in% only, added_as_dependency := TRUE]
  }
  checks[dependency_order(checks)]
}

# The given check ids plus everything they depend on (directly or indirectly).
with_dependencies <- function(ids, checks) {
  repeat {
    dependencies <- unlist(lapply(checks$depends_on[match(ids, checks$check_id)], split_list))
    new_ids <- setdiff(dependencies, ids)
    if (!length(new_ids)) return(ids)
    ids <- c(ids, new_ids)
  }
}

# Row order in which every check comes after its dependencies. Among the checks
# that are ready, the one highest in the workbook goes first, so the order stays
# close to the workbook. Stops when dependencies form a loop.
dependency_order <- function(checks) {
  ids <- checks$check_id
  dependencies <- lapply(checks$depends_on, function(text) intersect(split_list(text), ids))
  placed <- logical(length(ids))
  order <- integer()
  while (length(order) < length(ids)) {
    ready <- which(!placed & vapply(dependencies, function(d) all(d %in% ids[placed]), logical(1)))
    if (!length(ready)) {
      stop("The dependencies (depends_on) in the rules workbook form a loop between: ",
           paste(ids[!placed], collapse = ", "), call. = FALSE)
    }
    order <- c(order, ready[1])
    placed[ready[1]] <- TRUE
  }
  order
}

# --- Running -----------------------------------------------------------------------

run_checks <- function(plan, dataset, rules, check_functions, progress = FALSE) {
  progress <- make_progress(progress, nrow(plan))
  dataset$rules <- rules                            # field lists for the required-field checks
  dataset$cache <- new.env(parent = emptyenv())     # fresh links and inherited values for this run
  status_of <- character()               # check_id -> status, filled as the checks run
  check_rows <- vector("list", nrow(plan))
  issues_per_check <- vector("list", nrow(plan))

  for (i in seq_len(nrow(plan))) {
    check <- plan[i]
    progress(i - 1, check$title)
    outcome <- run_one_check(check, dataset, settings_for(rules, check$check_id),
                             check_functions[[check$check_id]], status_of)
    status_of[check$check_id] <- outcome$status
    check_rows[[i]] <- data.table(check_id = check$check_id, status = outcome$status,
                                  reason = outcome$reason, duration_s = outcome$duration_s)
    if (nrow(outcome$issues$problems)) {
      outcome$issues$problems <- apply_severity(outcome$issues$problems, check$check_id, check$severity)
      issues_per_check[[i]] <- outcome$issues
    }
  }
  progress(nrow(plan), "done")

  issues <- combine_issues(issues_per_check)
  if (!nrow(issues$problems)) issues$problems <- apply_severity(empty_problems(), character(), character())
  list(checks = rbindlist(check_rows), problems = issues$problems, rows = issues$rows,
       lookups = lookup_summary(dataset))
}

# Run one check and turn whatever happens into a status.
run_one_check <- function(check, dataset, settings, check_function, status_of) {
  result <- function(status, reason = "", issues = empty_issues(), duration_s = 0) {
    list(status = status, reason = reason, issues = issues, duration_s = duration_s)
  }

  if (check$severity == "off") return(result("switched off", "severity is 'off' in the rules workbook"))
  if (is.null(check_function)) return(result("not yet implemented", paste("planned for milestone", check$milestone)))
  if (!length(dataset$tables) && check$check_id != "archive_readable") {
    return(result("skipped", "the archive could not be read (see archive_readable)"))
  }

  blocked_by <- intersect(split_list(check$depends_on), names(status_of)[status_of %in% blocking_statuses])
  if (length(blocked_by)) {
    return(result("skipped", sprintf("needs %s, which could not run (%s)",
                                     paste(blocked_by, collapse = ", "), paste(status_of[blocked_by], collapse = ", "))))
  }

  notes <- character()
  started <- Sys.time()
  outcome <- tryCatch(
    withCallingHandlers(
      {
        issues <- check_function(dataset, settings)
        validate_issues(issues, check$check_id)
        result(if (nrow(issues$problems)) "issues found" else "passed", issues = issues)
      },
      warning = function(w) {
        notes <<- c(notes, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    qc_not_applicable = function(condition) result("not applicable", conditionMessage(condition)),
    qc_not_performed  = function(condition) result("not performed", conditionMessage(condition)),
    error             = function(condition) result("check error", conditionMessage(condition)))

  if (length(notes)) outcome$reason <- paste(c(outcome$reason[nzchar(outcome$reason)], unique(notes)), collapse = "; ")
  outcome$duration_s <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
  outcome
}

# Add check_id and the final severity to the problems of one check.
apply_severity <- function(problems, check_id, rule_severity) {
  problems <- copy(problems)
  final <- rep(rule_severity, nrow(problems))
  lower <- !is.na(problems$severity_hint) & severity_rank[problems$severity_hint] < severity_rank[final]
  final[lower] <- problems$severity_hint[lower]
  set(problems, j = "check_id", value = rep(check_id, nrow(problems)))
  set(problems, j = "severity", value = final)
  set(problems, j = "severity_hint", value = NULL)
  setcolorder(problems, c("problem", "check_id", "severity"))
  problems
}

validate_issues <- function(issues, check_id) {
  if (!inherits(issues, "qc_issues")) {
    stop("check_", check_id, "() did not return issues; use new_issues() or empty_issues()")
  }
}

# Progress: nothing, a console progress bar, or the caller's function (the app).
make_progress <- function(progress, total) {
  if (is.function(progress)) return(function(done, title) progress(done, total, title))
  if (isTRUE(progress) && total > 0) {
    bar <- utils::txtProgressBar(min = 0, max = total, style = 3)
    return(function(done, title) {
      utils::setTxtProgressBar(bar, done)
      if (done == total) close(bar)
    })
  }
  function(done, title) invisible(NULL)
}
