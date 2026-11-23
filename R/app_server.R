undepRcate_server <- function(input, output, session) {
  con <- db_open(getOption("undepRcate.db", getOption("rreps.db", ".state/progress.sqlite")))
  session$onSessionEnded(function() db_close(con))
  catalog <- exercise_catalog()
  current <- reactiveVal(active_session(con))
  item <- reactiveVal(NULL)
  result <- reactiveVal(NULL)
  finished <- reactiveVal(NULL)
  refresh <- reactiveVal(0L)
  bump <- function() refresh(isolate(refresh()) + 1L)
  progress_snapshot <- reactive({ refresh(); skill_progress(con) })
  pref <- function(name, default) get_setting(con, name, default)
  goal_time <- function() as.numeric(pref("speed_goal", "90"))
  title_for <- function(id) catalog$title[match(id, catalog$id)]
  protect <- function(expr) tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    if (grepl("another tab|current practice session|already passed", conditionMessage(e))) restore()
    showNotification(conditionMessage(e), type = "error", duration = 8)
    invisible(NULL)
  })

  show_item <- function(s) {
    current(s)
    result(NULL)
    item(if (is.null(s) || s$lesson_pending == 1L) NULL else make_exercise(s$template_id, s$seed, s$level))
    session$sendCustomMessage("rr-editor", list(token = rep_token(s),
      value = if (is.null(s)) "" else s$draft, passed = !is.null(s) && s$passed == 1L))
  }
  restore <- function() show_item(active_session(con))
  set_completion <- function(disabled) {
    shinyAce::updateAceEditor(session, "code", autoComplete = if (disabled) "disabled" else "enabled",
                              autoCompleters = "keyword")
  }
  session$onFlushed(function() {
    restore()
    updateNumericInput(session, "daily_goal", value = as.integer(pref("daily_goal", "20")))
    updateNumericInput(session, "speed_goal", value = goal_time())
    updateSelectInput(session, "accent", selected = pref("accent", "mint"))
    updateCheckboxInput(session, "show_timer", value = pref("show_timer", "yes") == "yes")
    disabled <- pref("disable_suggestions", "yes") == "yes"
    updateCheckboxInput(session, "disable_suggestions", value = disabled)
    set_completion(disabled)
    session$sendCustomMessage("rr-accent", pref("accent", "mint"))
  }, once = TRUE)

  output$active <- reactive(!is.null(current()))
  outputOptions(output, "active", suspendWhenHidden = FALSE)
  output$lesson_pending <- reactive(isTRUE(current()$lesson_pending == 1L))
  outputOptions(output, "lesson_pending", suspendWhenHidden = FALSE)
  output$mode_note <- renderText(mode_notes[[req(input$mode)]])
  observeEvent(input$category, {
    pool <- catalog[catalog$category == input$category, ]
    pool <- ordered_drills(pool)
    updateSelectInput(session, "skill", choices = drill_choices(pool, input$category),
                      selected = if ("grouped_summary" %in% pool$id) "grouped_summary" else pool$id[1L])
  })

  begin <- function(focus, mode, ordering, target) {
    if (identical(mode, "Guided")) ordering <- "Sequential"
    req(is.null(current()), mode %in% practice_modes, ordering %in% c("Sequential", "Randomized"),
        target %in% c(0L, 5L, 10L, 20L, 50L, 100L))
    p <- progress_snapshot()
    seed <- new_seed()
    id <- next_drill(focus, catalog, p, ordering, 1L, seed)
    s <- start_session(con, focus, mode, ordering, target, id, seed, drill_level(id, p))
    finished(NULL)
    show_item(s)
    updateNavbarPage(session, "tab", "Practice")
  }
  observeEvent(input$start, protect(begin(req(input$skill), input$mode, input$ordering, as.integer(input$target))))
  open_review <- function(id) protect({
    if (!is.null(current())) {
      showNotification("Finish the active session before starting a review.")
    } else begin(id, "Recall", "Randomized", 5L)
  })
  analytics_server(input, output, session, con, catalog, refresh, open_review)

  output$session_heading <- renderUI({
    s <- req(current())
    refresh()
    if (s$lesson_pending == 1L) {
      description <- if (s$target == 0L) "Unlimited sequential practice" else
        sprintf("%d sequential repetitions", s$target)
      return(div(class = "session-top", div(span(class = "eyebrow", "Guided lesson"), h1(title_for(s$template_id))),
        span(class = "lesson-plan", description, " after the lesson")))
    }
    target <- if (s$target == 0L) "Unlimited" else s$target
    tagList(div(class = "session-top",
      div(span(class = "eyebrow", paste(s$mode, "practice")), h1(title_for(s$template_id))),
      div(class = "rep-meter", span(sprintf("Rep %d / %s", s$position, target)),
        if (s$target > 0L) tags$progress(max = s$target, value = s$position - 1L + s$passed),
        if (s$mode == "Speed" || pref("show_timer", "yes") == "yes")
          span(class = "timer", `data-start` = s$item_started_at,
               `data-stopped` = if (s$passed) "1" else "0",
               `data-target` = if (s$mode == "Speed") goal_time() else 0, "\u2014"))))
  })
  example_retry <- reactiveVal(0L)
  lesson_input <- reactive({
    s <- req(current())
    req(s$lesson_pending == 1L)
    guided_example(s)
  })
  worked <- reactive({
    ex <- lesson_input()
    example_retry()
    list(ex = ex, answer = tryCatch(run_worked_example(ex), error = identity))
  })
  observeEvent(input$retry_example, {
    s <- req(current())
    req(s$lesson_pending == 1L, identical(input$retry_example, lesson_token(s)))
    example_retry(example_retry() + 1L)
  })
  output$guided_lesson <- renderUI({
    example <- worked()
    guided_lesson_ui(example$ex, example$answer, current())
  })
  observeEvent(input$finish_lesson, protect({
    s <- req(current())
    req(s$lesson_pending == 1L, identical(input$finish_lesson, lesson_token(s)))
    show_item(finish_guided_lesson(con, s))
  }))
  output$prompt <- renderUI(p(req(item())$instructions))
  output$preview <- renderUI({
    inputs <- req(item())$inputs
    tagList(lapply(names(inputs), function(nm) preview_input(inputs[[nm]], nm)))
  })
  output$file_info <- renderUI({
    ex <- req(item())
    s <- isolate(current())
    path <- prepare_files(s, ex)
    div(class = "file-instructions",
      p("From the project terminal, open this repetition in Positron."),
      pre(paste0('positron -r "', gsub("\\\\", "/", path), '"')),
      p("Load the inputs from the project R console."),
      pre(sprintf('source("%s")', file.path(dirname(path), "context.R"))),
      p("Edit and save answer.R, then select Check answer here."),
      tags$small("Use the final expression in answer.R to return your answer."))
  })
  output$skill_status <- renderUI({
    s <- req(current())
    refresh()
    p <- progress_snapshot()
    row <- p[p$template_id == s$template_id, ]
    checkpoint <- if (s$mode == "Mastery") {
      run <- mastery_checkpoint(con, s$id, s$template_id)
      div(class = "checkpoint", strong(if (run$best >= 3L) "Checkpoint reached" else "Session checkpoint"),
          p(sprintf("%d of 3 consecutive unaided first passes on new variations of this drill.", min(run$current, 3L))),
          tags$progress(max = 3L, value = min(run$current, 3L)))
    }
    tagList(if (!nrow(row)) p("Beginner \u00b7 No graded attempts yet.") else
      tagList(strong(row$level), p(sprintf("%d independent first passes \u00b7 %d consecutive \u00b7 %d later review days",
        row$unseen_successes, row$streak, row$retention_days)),
        p(class = "muted", paste("Next review", format(as.POSIXct(row$due_at, origin = "1970-01-01"),
                                                        "%b %d at %H:%M", tz = "")))), checkpoint)
  })
  output$hint_text <- renderUI({
    s <- req(current())
    ex <- req(item())
    if (s$hints == 0L) return(p(class = "muted", "Reveal a hint if you need one. Hints are recorded for this repetition."))
    tagList(lapply(seq_len(s$hints), function(i) div(class = "revealed",
      if (i == 4L) span(`data-last-hint` = "true"), strong(c("Concept", "Technique", "Partial example", "Worked answer")[i]), pre(ex$hints[i]))))
  })
  observeEvent(input$hint, protect({
    s <- req(current())
    req(s$lesson_pending == 0L, s$hints < 4L, !s$passed)
    current(save_draft(con, s, s$draft, s$hints + 1L))
  }))
  observeEvent(input$draft_event, protect({
    s <- req(current())
    payload <- input$draft_event
    req(s$lesson_pending == 0L, identical(payload$token, rep_token(s)), !s$passed, !identical(s$draft, payload$code))
    current(save_draft(con, s, payload$code))
  }))
  observeEvent(input$submission, protect({
    s <- req(current())
    payload <- input$submission
    req(s$lesson_pending == 0L, identical(payload$token, rep_token(s)), !s$passed)
    code <- if (identical(payload$source, "Positron")) read_practice_file(s) else payload$code
    if (!nzchar(trimws(code))) {
      showNotification("Write an answer and submit it to run the tests.")
      return()
    }
    seconds <- max(0, as.numeric(Sys.time()) - s$item_started_at)
    result_value <- withProgress(message = "Checking R code", detail = "Running preview and hidden fixtures",
                                 value = 0.5, assess_answer(item(), code))
    updated <- record_attempt(con, s, code, s$hints, result_value$status, seconds, result_value$elapsed)
    current(updated)
    result(list(review = result_value, seconds = seconds, hints = s$hints))
    bump()
    if (identical(payload$source, "Positron")) {
      session$sendCustomMessage("rr-editor", list(token = rep_token(updated),
        value = code, passed = updated$passed == 1L))
    } else if (updated$passed) session$sendCustomMessage("rr-passed", rep_token(updated))
  }))
  output$feedback <- renderUI({
    s <- req(current())
    x <- result()
    feedback_ui(x$review, x$hints, s$passed, x$seconds,
                if (s$mode == "Speed") goal_time(), item()$explanation)
  })
  summarize_session <- function(id) {
    history <- attempt_history(con, include_code = FALSE)
    rows <- history[history$session_id == id & history$status != "infrastructure_error", ]
    finished(sprintf("%d repetitions passed across %d submissions.", sum(rows$status == "pass"), nrow(rows)))
    bump()
  }
  observeEvent(input$advance, protect({
    s <- req(current())
    req(s$passed == 1L)
    p <- progress_snapshot()
    seed <- new_seed(s)
    id <- next_drill(s$focus, catalog, p, s$ordering, s$position + 1L, seed)
    updated <- advance_session(con, s, id, seed, drill_level(id, p))
    show_item(updated)
    if (is.null(updated)) summarize_session(s$id)
  }))
  observeEvent(input$end, {
    req(current())
    showModal(modalDialog(title = "End this session?", "Your submitted attempts stay in your history.",
      footer = tagList(modalButton("Keep practicing"), actionButton("end_confirm", "End session", class = "btn-primary"))))
  })
  observeEvent(input$end_confirm, protect({
    removeModal()
    s <- req(current())
    end_session(con, s)
    show_item(NULL)
    summarize_session(s$id)
  }))
  output$finished <- renderUI({
    message <- req(finished())
    div(class = "finished", strong("Session complete"), span(message))
  })
  observeEvent(input$save_prefs, protect({
    valid_number <- function(x, low, high) length(x) == 1L && is.finite(x) && x == round(x) && x >= low && x <= high
    if (!valid_number(input$daily_goal, 1L, 1000L) || !valid_number(input$speed_goal, 5L, 3600L))
      stop("Enter a daily goal from 1 to 1,000 and a speed target from 5 to 3,600 whole seconds.")
    req(input$accent %in% c("mint", "blue", "amber"))
    set_setting(con, "daily_goal", input$daily_goal)
    set_setting(con, "speed_goal", input$speed_goal)
    set_setting(con, "show_timer", if (input$show_timer) "yes" else "no")
    set_setting(con, "accent", input$accent)
    disabled <- isTRUE(input$disable_suggestions)
    set_setting(con, "disable_suggestions", if (disabled) "yes" else "no")
    set_completion(disabled)
    session$sendCustomMessage("rr-accent", input$accent)
    bump()
    showNotification("Preferences saved.", type = "message")
  }))
}
