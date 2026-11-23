# Teaching examples use the preceding seed so the learner starts practice on
# fresh inputs and can reopen the same lesson after restarting the application.
guided_example <- function(s) {
  example_seed <- as.integer((as.double(s$seed) + 1000000000) %% 1000000001)
  make_exercise(s$template_id, example_seed, s$level, preview_only = TRUE, task_seed = s$seed)
}

guided_notes <- function(intuition, steps, math, symbols) {
  list(intuition = intuition, steps = steps, math = math, symbols = symbols)
}

lesson_notes <- function(ex) {
  if (!is.null(ex$lesson)) return(ex$lesson)
  notes <- if (ex$category == "Missing Data") guided_missing_notes(ex) else guided_core_notes(ex)
  if (!is.null(notes)) return(notes)
  # Older extension drills already supply a concept, technique and explanation.
  guided_notes(ex$hints[1L], c(ex$hints[2L], ex$explanation), NULL, list())
}

lesson_token <- function(s) paste("lesson", s$id, s$position, s$seed, sep = "-")

# Only authored reference code and the displayed example inputs cross this
# boundary. Learner submissions continue through the ordinary three-fixture grader.
run_worked_example <- function(ex) {
  callr::r(function(solution, inputs, seed, category, kind, cases, prompt) {
    suppressPackageStartupMessages(library(tidyverse))
    if (category == "Missing Data") loadNamespace("mice")
    set.seed(as.integer((as.double(seed) + 1) %% .Machine$integer.max))
    value <- eval(parse(text = solution), list2env(inputs, parent = parent.env(globalenv())))
    if (kind == "plot") {
      return(htmltools::plotTag(print(value), alt = prompt, width = 640, height = 400,
        device = grDevices::png, suppressSize = "xy"))
    }
    if (kind == "function") {
      return(data.frame(
        Arguments = vapply(cases, function(args) paste(vapply(args, deparse1, ""), collapse = ", "), ""),
        Returns = vapply(cases, function(args) deparse1(do.call(value, args)), "")
      ))
    }
    value
  }, args = list(ex$solution, ex$inputs, ex$seed, ex$category, ex$contract$kind,
                  ex$contract$cases, ex$instructions),
  libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE,
  timeout = getOption("undepRcate.timeout", getOption("rreps.timeout", 15)))
}

guided_preview <- function(value, label) {
  if (inherits(value, "shiny.tag")) return(value)
  if (inherits(value, "mids")) return(mids_preview(value, label, guided_preview))
  if (is.matrix(value)) {
    frame <- as.data.frame(value)
    if (!is.null(rownames(value))) frame <- cbind(Row = rownames(value), frame)
    return(guided_preview(frame, label))
  }
  if (is.character(value) && length(value) == 1L && !is.na(value) && grepl("\n", value, fixed = TRUE)) {
    return(div(class = "fixture", h3(code(label)), pre(value)))
  }
  if (is.list(value) && !is.data.frame(value) && !is.object(value) &&
      length(value) && !is.null(names(value)) &&
      all(vapply(value, function(x) is.character(x) && length(x) == 1L, logical(1)))) {
    return(tagList(lapply(names(value), function(nm) guided_preview(value[[nm]], paste0(label, "$", nm)))))
  }
  full <- if (is.data.frame(value) && nrow(value) > 5L) {
    tags$details(tags$summary(sprintf("Show all %d rows", nrow(value))),
      pre(paste(capture.output(print(as.data.frame(value), row.names = FALSE,
                                     max = length(value) * nrow(value))), collapse = "\n")))
  } else if (is.atomic(value) && length(value) > 12L) {
    tags$details(tags$summary(sprintf("Show all %d values", length(value))),
      pre(paste(capture.output(print(value, max = length(value))), collapse = "\n")))
  }
  tagList(preview_input(value, label), full)
}

notation_ui <- function(notes) {
  if (!length(notes$math)) return(NULL)
  div(class = "panel-card lesson-notation", h2("The idea in notation"),
    p(class = "lesson-formula", notes$math),
    div(class = "table-scroll", tabindex = 0, tags$table(
      tags$thead(tags$tr(lapply(c("Symbol", "Meaning in this example", "Dimensions", "Status", "Conditioning"),
        tags$th, scope = "col"))),
      tags$tbody(lapply(notes$symbols, function(row) tags$tr(
        tags$th(scope = "row", row[[1L]]), lapply(row[-1L], tags$td)))))))
}

guided_lesson_ui <- function(ex, answer, s) {
  notes <- lesson_notes(ex)
  result <- if (inherits(answer, "error")) {
    div(role = "status", p("The example output could not be displayed."),
      tags$details(tags$summary("Execution details"), pre(conditionMessage(answer))),
      tags$button(type = "button", id = "retry_example", class = "btn btn-default",
        `data-lesson-token` = lesson_token(s), "Run the worked answer again"))
  } else guided_preview(answer, "Result")
  tagList(
    div(class = "lesson-grid",
      div(class = "panel-card lesson-walkthrough", h2("Worked example"), p(class = "lesson-task", ex$instructions),
        h3("The idea"), p(notes$intuition), h3("Walk through the example"),
        tags$ol(lapply(notes$steps, tags$li))),
      div(class = "panel-card lesson-inputs", h2("Example inputs"),
        lapply(names(ex$inputs), function(nm) guided_preview(ex$inputs[[nm]], nm)))),
    div(class = "panel-card lesson-answer", h2("Worked answer"), pre(code(ex$solution), .noWS = "inside"),
      h3("What the code returns"), result),
    notation_ui(notes),
    div(class = "lesson-next", tags$button(type = "button", id = "finish_lesson", class = "btn btn-primary",
      `data-lesson-token` = lesson_token(s), "Finish lesson and start practice \u2192")))
}
