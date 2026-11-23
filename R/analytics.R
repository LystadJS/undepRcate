home_insights_ui <- function() {
  div(class = "work-side", uiOutput("home_daily_goal"), uiOutput("home_review_queue"))
}

analytics_ui <- function(catalog) {
  tagList(
    div(class = "section-head", h1("Practice history")),
    uiOutput("history_current"),
    div(class = "panel-card",
        h2("Training log"),
        fluidRow(
          column(3, selectInput("history_category", "Category",
                                c("All categories", sort(unique(catalog$category))))),
          column(3, selectInput("history_template", "Exercise",
                                c("All exercises" = "", setNames(catalog$id, catalog$title)))),
          column(3, selectInput("history_mode", "Practice mode",
                                c("All modes", "Recall", "Guided", "Speed", "Mastery"))),
          column(3, selectInput("history_period", "Period",
                                c("All dates", "Last 7 days", "Last 30 days", "Custom dates")))
        ),
        conditionalPanel("input.history_period === 'Custom dates'",
          dateRangeInput("history_dates", "Practice dates", start = Sys.Date() - 29, end = Sys.Date())
        ),
        uiOutput("history_metrics"), uiOutput("history_graphs"),
        uiOutput("history_by_skill"), uiOutput("history_submissions")),
    div(class = "panel-card", h2("Skill mastery"),
        p(class = "muted", "Current level for each exercise, using your full practice history."),
        uiOutput("history_heatmap")),
    fluidRow(column(6, uiOutput("history_review_queue")),
             column(6, uiOutput("history_accomplishments")))
  )
}

analytic_stat <- function(label, value) {
  div(class = "stat", tags$span(class = "muted", label), tags$strong(value))
}

practice_metrics <- function(history) {
  passes <- history$status == "pass"
  list(completed = sum(passes), attempts = nrow(history),
       accuracy = if (length(passes)) mean(passes) else NA_real_,
       median_seconds = if (any(passes)) stats::median(history$seconds[passes]) else NA_real_)
}

metric_percent <- function(value) {
  if (is.na(value)) "\u2014" else sprintf("%.0f%%", 100 * value)
}

metric_seconds <- function(value) {
  if (is.na(value)) "\u2014" else if (value < 60) sprintf("%.1f s", value) else
    sprintf("%d min %02d s", floor(round(value) / 60), as.integer(round(value) %% 60))
}

practice_filter <- function(history, catalog, category, template, mode, range = NULL) {
  keep <- history$status != "infrastructure_error"
  if (length(category) && category != "All categories") {
    keep <- keep & history$template_id %in% catalog$id[catalog$category == category]
  }
  if (length(template) && nzchar(template)) keep <- keep & history$template_id == template
  if (length(mode) && mode != "All modes") keep <- keep & tolower(history$mode) == tolower(mode)
  if (length(range) == 2L && !anyNA(range)) {
    days <- as.Date(history$practice_date)
    keep <- keep & days >= range[1L] & days <= range[2L]
  }
  history[keep, , drop = FALSE]
}

recent_milestones <- function(history, catalog) {
  history <- history[history$status == "pass", , drop = FALSE]
  if (!nrow(history)) return(data.frame(date = character(), label = character()))
  marks <- c(1L, 10L, 20L, 50L, 100L, 250L, 500L, 1000L)
  marks <- marks[marks <= nrow(history)]
  total <- data.frame(at = history$attempted_at[marks], date = history$practice_date[marks],
                      label = ifelse(marks == 1L, "First repetition completed",
                                     paste(marks, "repetitions completed")))
  first <- !duplicated(history$template_id)
  titles <- catalog$title[match(history$template_id[first], catalog$id)]
  titles[is.na(titles)] <- history$template_id[first][is.na(titles)]
  firsts <- data.frame(at = history$attempted_at[first], date = history$practice_date[first],
                       label = paste("First pass in", titles))
  events <- rbind(total, firsts)
  utils::head(events[order(-events$at), c("date", "label")], 5L)
}

analytics_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#e4e9eb"),
      axis.text = ggplot2::element_text(colour = "#bec8cb"),
      axis.title = ggplot2::element_text(colour = "#bec8cb"),
      strip.text = ggplot2::element_text(colour = "#e4e9eb", face = "bold", hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#373f41"),
      plot.background = ggplot2::element_rect(fill = "#1b1e21", colour = NA),
      legend.position = "none"
    )
}

analytics_server <- function(input, output, session, con, catalog, refresh, open_review) {
  # Update calendar-bound metrics even when an app remains open across midnight.
  clock <- reactive({ invalidateLater(60000, session); Sys.time() })
  today <- reactive(as.Date(format(clock(), "%Y-%m-%d")))
  # SQLite changes the connection-local data_version when ANOTHER connection
  # commits. The explicit counter covers writes on this browser's connection.
  history <- reactivePoll(10000, session,
    checkFunc = function() list(refresh(), DBI::dbGetQuery(con, "PRAGMA data_version")[[1L]]),
    valueFunc = function() {
      h <- attempt_history(con, include_code = FALSE)
      h[h$status != "infrastructure_error", , drop = FALSE]
    })
  progress <- reactive(skill_progress(con, history = history()))
  chart_accent <- reactive({
    history()
    switch(get_setting(con, "accent", "mint"), blue = "#94c5ff", amber = "#eac186", "#80d5c0")
  })
  title_for <- function(id) {
    label <- catalog$title[match(id, catalog$id)]
    ifelse(is.na(label), id, label)
  }
  observeEvent(input$review_template, {
    id <- input$review_template
    if (id %in% catalog$id) open_review(id)
  })
  due_component <- function() {
    progress <- progress()
    due <- progress[progress$due_at <= as.numeric(clock()), , drop = FALSE]
    priority <- review_priority(due$due_at, due$recent_error_rate, clock())
    due <- due[order(-priority, due$due_at, due$template_id), , drop = FALSE]
    div(class = "panel-card", h2("Skills due for review"),
        if (!nrow(due)) {
          p(class = "muted", if (nrow(progress)) "You're up to date with scheduled reviews."
            else "Practice a skill to schedule its first review.")
        } else {
          tagList(lapply(seq_len(nrow(due)), function(i) {
            div(style = "display:flex;align-items:center;justify-content:space-between;gap:1rem;margin:.6rem 0",
                div(tags$strong(title_for(due$template_id[i])), tags$br(),
                    tags$small(class = "muted", due$level[i])),
                tags$button(type = "button", class = "btn btn-outline-primary",
                  `data-template` = due$template_id[i], `aria-label` = paste("Review", title_for(due$template_id[i])),
                  style = "color:var(--accent);border-color:var(--accent);background:transparent",
                  onclick = "Shiny.setInputValue('review_template',this.dataset.template,{priority:'event'})",
                  "Review"))
          }))
        })
  }
  output$home_review_queue <- renderUI(due_component())
  output$history_review_queue <- renderUI(due_component())
  output$home_daily_goal <- renderUI({
    h <- history()
    target <- suppressWarnings(as.integer(get_setting(con, "daily_goal", "20")))
    if (is.na(target) || target < 1L) target <- 20L
    count <- sum(h$practice_date == as.character(today()) & h$status == "pass")
    div(class = "panel-card", h2("Daily goal"),
        p(tags$strong(style = "font-size:1.9rem", count), tags$span(class = "muted", paste(" of", target, "repetitions"))),
        tags$progress(style = "width:100%", max = target,
                      value = min(count, target), `aria-label` = "Daily goal progress"),
        p(class = "muted", if (count >= target) "Daily goal reached."
          else sprintf("%d more to reach today's goal.", target - count)))
  })
  output$history_current <- renderUI({
    h <- history()
    streaks <- practice_streaks(h$practice_date, today())
    day_label <- function(count) paste(count, if (count == 1L) "day" else "days")
    div(class = "stat-grid", analytic_stat("Lifetime repetitions", sum(h$status == "pass")),
        analytic_stat("Current streak", day_label(streaks["current"])),
        analytic_stat("Longest streak", day_label(streaks["longest"])))
  })
  chosen <- reactive({
    range <- switch(if (is.null(input$history_period)) "All dates" else input$history_period,
                    "Last 7 days" = c(today() - 6, today()), "Last 30 days" = c(today() - 29, today()),
                    "Custom dates" = input$history_dates, NULL)
    practice_filter(history(), catalog, input$history_category, input$history_template,
                    input$history_mode, range)
  })
  output$history_metrics <- renderUI({
    m <- practice_metrics(chosen())
    tagList(div(class = "stat-grid", analytic_stat("Completed", m$completed),
                analytic_stat("Submissions", m$attempts), analytic_stat("Accuracy", metric_percent(m$accuracy)),
                analytic_stat("Median solve time", metric_seconds(m$median_seconds))),
            p(class = "muted", "Accuracy counts all graded submissions, including retries. Solve time measures elapsed time from the prompt to its first passing submission."))
  })
  output$history_graphs <- renderUI({
    if (!nrow(chosen())) return(p(class = "muted", "No submissions match this training log."))
    tagList(h3("Daily practice"), plotOutput("activity_plot", height = 190),
            h3("Session trends"), plotOutput("trend_plot", height = 300))
  })
  output$activity_plot <- renderPlot({
    h <- chosen()
    req(nrow(h) > 0)
    daily <- dplyr::count(h, day = as.Date(practice_date), name = "submissions")
    ggplot2::ggplot(daily, ggplot2::aes(day, submissions)) +
      ggplot2::geom_col(fill = chart_accent(), width = 0.75) +
      ggplot2::scale_x_date(date_labels = "%b %d") +
      ggplot2::labs(x = NULL, y = "Submissions") + analytics_plot_theme()
  }, res = 96, alt = "Number of submissions on each practice date")
  output$trend_plot <- renderPlot({
    h <- chosen()
    req(nrow(h) > 0)
    series <- h |>
      dplyr::group_by(session_id) |>
      dplyr::summarise(accuracy = mean(status == "pass") * 100,
                       seconds = stats::median(seconds[status == "pass"]), .groups = "drop") |>
      tidyr::pivot_longer(c("accuracy", "seconds"), names_to = "metric", values_to = "value")
    series$metric <- factor(series$metric, levels = c("accuracy", "seconds"),
                            labels = c("Accuracy (%)", "Median solve time (seconds)"))
    # A session without a pass has no solve time; leave a gap in that series.
    series <- dplyr::group_by(series, metric) |>
      dplyr::mutate(segment = cumsum(is.na(value))) |>
      dplyr::ungroup()
    lines <- series |>
      dplyr::filter(!is.na(value)) |>
      dplyr::group_by(metric, segment) |>
      dplyr::filter(dplyr::n() > 1L) |>
      dplyr::ungroup()
    ggplot2::ggplot(series, ggplot2::aes(session_id, value)) +
      ggplot2::geom_line(data = lines, ggplot2::aes(group = segment), colour = chart_accent()) +
      ggplot2::geom_point(colour = chart_accent(), size = 2, na.rm = TRUE) +
      ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_y") +
      ggplot2::scale_x_continuous(breaks = function(limits) {
        intersect(unique(series$session_id), pretty(limits, n = 6))
      }) + ggplot2::labs(x = "Session", y = NULL) + analytics_plot_theme()
  }, res = 96, alt = "Accuracy and median solve time per session")
  output$history_by_skill <- renderUI({
    h <- chosen()
    if (!nrow(h)) return(NULL)
    groups <- split(h, h$template_id)
    rows <- lapply(groups, function(part) {
      m <- practice_metrics(part)
      tags$tr(tags$th(scope = "row", title_for(part$template_id[1L])),
              tags$td(m$completed), tags$td(metric_percent(m$accuracy)), tags$td(metric_seconds(m$median_seconds)))
    })
    tagList(h3("By exercise"),
            div(class = "table-responsive", tags$table(class = "table",
              tags$thead(tags$tr(lapply(c("Exercise", "Completed", "Accuracy", "Median solve"),
                                       function(label) tags$th(scope = "col", label)))),
              tags$tbody(unname(rows)))))
  })
  output$history_heatmap <- renderUI({
    p <- progress()
    levels <- p$level[match(catalog$id, p$template_id)]
    levels[is.na(levels)] <- "Unpracticed"
    fills <- c(Unpracticed = "#252b2e", Beginner = "#35404a", Developing = "#244843",
               Proficient = "#246052", Automatic = "#237061")
    tagList(lapply(unique(catalog$category), function(group) {
      idx <- which(catalog$category == group)
      div(style = "margin-bottom:1.1rem", h3(group),
          div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(155px,1fr));gap:5px",
              lapply(idx, function(i) {
                div(style = sprintf("background:%s;color:#f0f4f4;padding:.55rem .7rem;border-radius:4px", fills[levels[i]]),
                    tags$strong(catalog$title[i]), tags$br(), tags$small(levels[i]))
              })))
    }))
  })
  output$history_submissions <- renderUI({
    h <- chosen()
    if (!nrow(h)) return(NULL)
    recent <- utils::head(h[order(-h$attempted_at, -h$id), , drop = FALSE], 15L)
    recent$code <- recent_answer_code(con, recent$id)
    tagList(h3("Recent submissions"), lapply(seq_len(nrow(recent)), function(i) {
      row <- recent[i, ]
      stamp <- format(as.POSIXct(row$attempted_at, origin = "1970-01-01"), "%b %d %H:%M", tz = "")
      status <- switch(row$status, pass = "Passed", incorrect = "Incorrect", syntax_error = "Syntax error",
                       runtime_error = "Runtime error", timeout = "Timed out", row$status)
      tags$details(style = "border-bottom:1px solid #373f41;padding:.55rem 0",
        tags$summary(tags$strong(status), " \u00b7 ", title_for(row$template_id), " \u00b7 ", stamp),
        tags$small(class = "muted", paste0(row$mode, " \u00b7 Rep ", row$position, " \u00b7 ",
                                           metric_seconds(row$seconds), " \u00b7 Hints used: ", row$hints,
                                           if (isTRUE(row$guide_shown == 1L)) " \u00b7 Typing guide shown")),
        tags$pre(tags$code(row$code)))
    }))
  })
  output$history_accomplishments <- renderUI({
    events <- recent_milestones(history(), catalog)
    div(class = "panel-card", h2("Recent accomplishments"),
        if (!nrow(events)) p(class = "muted", "Your first passing submission starts this record.")
        else tags$ul(lapply(seq_len(nrow(events)), function(i) {
          tags$li(events$label[i], tags$br(), tags$small(class = "muted", events$date[i]))
        })))
  })
}
