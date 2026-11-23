drill_level <- function(id, progress) {
  earned <- progress$unseen_successes[match(id, progress$template_id)]
  if (is.na(earned)) return(1L)
  if (earned >= 8L) 3L else if (earned >= 3L) 2L else 1L
}

next_drill <- function(focus, catalog, progress, ordering, position, seed) {
  if (focus %in% catalog$id) return(focus)
  pool <- catalog[catalog$category == sub("^category/", "", focus), ]
  stopifnot(nrow(pool) > 0L)
  successes <- sum(progress$unseen_successes[progress$template_id %in% pool$id])
  available <- min(3L, 1L + as.integer(successes >= 3L) + as.integer(successes >= 8L))
  pool <- pool[pool$difficulty <= max(min(pool$difficulty), available), ]
  pool <- ordered_drills(pool)
  if (ordering == "Randomized") return(withr::with_seed(seed, sample(pool$id, 1L)))
  pool$id[1L + (position - 1L) %% nrow(pool)]
}

new_seed <- function(s = NULL) {
  if (is.null(s) || s$ordering == "Randomized") return(sample.int(1000000000L, 1L))
  as.integer((as.double(s$seed) + 1) %% 1000000001)
}

rep_token <- function(s) {
  if (is.null(s) || isTRUE(s$lesson_pending == 1L)) return("")
  paste(s$id, s$position, s$seed, sep = "-")
}

mids_preview <- function(value, name, table_view) {
  div(class = "fixture", h3(code(name), tags$small("mice mids object")),
    p(sprintf("%d imputations, %d completed iterations. The table shows the original incomplete data.",
              value$m, value$iteration)), table_view(value$data, paste0(name, "$data")))
}

preview_input <- function(value, name) {
  if (inherits(value, "mids")) {
    return(mids_preview(value, name, preview_input))
  }
  if (is.data.frame(value)) {
    types <- vapply(value, function(column) paste(class(column), collapse = "/"), "")
    rows <- utils::head(value, 5L)
    return(div(class = "fixture", h3(code(name), tags$small(sprintf("%d rows \u00b7 %d columns", nrow(value), ncol(value)))),
      div(class = "table-scroll", tabindex = 0, tags$table(
        tags$thead(tags$tr(lapply(names(value), function(col) tags$th(col, tags$small(types[[col]]))))),
        tags$tbody(lapply(seq_len(nrow(rows)), function(i)
          tags$tr(lapply(rows, function(x) {
            cell <- x[i]
            label <- as.character(cell)
            if (is.double(cell) && !is.object(cell) && is.finite(cell) && cell != trunc(cell)) {
              label <- format(signif(cell, 5L), trim = TRUE)
            }
            tags$td(title = as.character(cell), label)
          }))))))))
  }
  div(class = "fixture", h3(code(name), tags$small(paste(class(value)[1L], "\u00b7 length", length(value)))),
      pre(paste(capture.output(print(utils::head(value, 12L))), collapse = "\n")))
}

feedback_ui <- function(result, hints, passed, seconds, speed_goal, explanation) {
  titles <- c(pass = "Passed", incorrect = "Try again", syntax_error = "Syntax error",
              runtime_error = "Runtime error", timeout = "Time limit exceeded",
              infrastructure_error = "Grader error")
  if (is.null(result)) {
    if (isTRUE(as.logical(passed))) return(div(class = "panel-card feedback success",
        h3("Passed"), p("This result was saved. Continue to the next repetition.")))
    return(NULL)
  }
  ok <- result$status == "pass"
  checks <- result$checks
  failed <- checks[!checks$passed, , drop = FALSE]
  speed <- if (!is.null(speed_goal))
    sprintf(" \u00b7 %s target by %.1f s", if (seconds <= speed_goal) "Under" else "Over", abs(speed_goal - seconds))
  div(class = paste("panel-card feedback", if (ok) "success" else "retry"),
    h3(unname(titles[result$status])),
    p(class = "muted", sprintf("%d/%d checks passed \u00b7 %.2f s grading \u00b7 %.1f s to submit \u00b7 %d %s%s",
      sum(checks$passed), nrow(checks), result$elapsed, seconds, hints,
      if (hints == 1L) "hint" else "hints", paste(speed, collapse = ""))),
    if (ok) p(explanation) else p(paste(head(unique(failed$detail), 3L), collapse = " ")),
    tags$details(tags$summary("Test results"), tags$ul(class = "checklist",
      lapply(seq_len(nrow(checks)), function(i) tags$li(
        span(if (checks$passed[i]) "Pass" else "Fail", class = if (checks$passed[i]) "pass" else "fail"),
        checks$label[i], if (!checks$passed[i]) paste(" \u2014", checks$detail[i]))))),
    if (length(result$warnings)) tags$details(tags$summary("Warnings"), pre(paste(result$warnings, collapse = "\n"))),
    if (length(result$output)) tags$details(tags$summary("Console output"), pre(paste(result$output, collapse = "\n"))))
}
