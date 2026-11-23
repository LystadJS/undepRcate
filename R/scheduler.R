# Reviews open at local midnight so a morning session can review yesterday's work.
next_review_midnight <- function(day, days) {
  as.numeric(as.POSIXct(paste(as.Date(day) + days, "00:00:00"), tz = ""))
}

# Replay a single template's chronological attempt log. A clean success must be
# unaided on the first encounter with that template and seed. Raising difficulty
# on an old seed may leave some templates' data unchanged.
schedule_skill <- function(history, now = Sys.time()) {
  history <- history[history$status != "infrastructure_error", , drop = FALSE]
  if (!nrow(history)) return(NULL)
  history <- history[order(history$attempted_at, history$id), , drop = FALSE]
  passed <- history$status == "pass"
  first <- !duplicated(history[c("template_id", "seed")])
  assisted <- history$hints > 0L
  if (!is.null(history$guide_shown)) assisted <- assisted | history$guide_shown == 1L
  clean <- passed & first & !assisted
  intervals <- c(1L, 3L, 7L, 14L, 30L, 60L)
  step <- 1L
  streak <- 0L
  retained <- 0L
  due <- NA_real_
  last_clean_day <- ""
  for (i in seq_len(nrow(history))) {
    stamp <- history$attempted_at[i]
    day <- history$practice_date[i]
    if (!passed[i]) {
      streak <- 0L
      step <- 1L
      due <- stamp + 600
    } else if (clean[i]) {
      streak <- streak + 1L
      if (is.na(due) || last_clean_day == "") {
        due <- next_review_midnight(day, intervals[step])
      } else if (stamp >= due && day > last_clean_day) {
        retained <- retained + 1L
        step <- min(step + 1L, length(intervals))
        due <- next_review_midnight(day, intervals[step])
      } else if (stamp >= due) {
        # A same-day repair schedules tomorrow without extending the interval.
        due <- next_review_midnight(day, 1L)
      }
      last_clean_day <- day
    } else {
      # Familiar or assisted work earns no first-encounter or retention credit.
      if (assisted[i]) streak <- 0L
      if (is.na(due)) due <- stamp + 600
    }
  }
  independent <- sum(clean)
  mastery <- if (streak >= 10 && independent >= 15 && retained >= 3) {
    "Automatic"
  } else if (streak >= 5 && independent >= 8 && retained >= 1) {
    "Proficient"
  } else if (streak >= 3 && independent >= 3) {
    "Developing"
  } else {
    "Beginner"
  }
  # Due skills rank ahead of future reviews. Within each group, weigh recent
  # errors and overdue time. Cap lateness so an old skill cannot dominate forever.
  recent_error_rate <- mean(!utils::tail(passed, 10L))
  priority <- review_priority(due, recent_error_rate, now)
  data.frame(template_id = history$template_id[1L], attempts = nrow(history),
             correct = sum(passed), unseen_successes = independent, streak = streak,
             retention_days = retained, level = mastery, due_at = due,
             last_at = utils::tail(history$attempted_at, 1L), recent_error_rate = recent_error_rate,
             priority = priority)
}

review_priority <- function(due, recent_error_rate, now) {
  delta_seconds <- as.numeric(now) - due
  100 * (delta_seconds >= 0) + 10 * recent_error_rate + pmin(30, pmax(0, delta_seconds / 86400))
}

skill_progress <- function(con, now = Sys.time(), history = NULL) {
  if (is.null(history)) {
    history <- DBI::dbGetQuery(con, "SELECT id,template_id,seed,hints,guide_shown,status,attempted_at,practice_date
                                      FROM attempts WHERE status!='infrastructure_error'")
  }
  rows <- lapply(split(history, history$template_id), schedule_skill, now = now)
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(
    template_id = character(), attempts = integer(), correct = integer(),
    unseen_successes = integer(), streak = integer(), retention_days = integer(),
    level = character(), due_at = numeric(), last_at = numeric(), recent_error_rate = numeric(), priority = numeric()
  ))
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result[order(-result$priority, result$due_at, result$template_id), , drop = FALSE]
}

practice_streaks <- function(practice_dates, today = Sys.Date()) {
  days <- sort(unique(as.integer(as.Date(practice_dates))))
  days <- days[!is.na(days) & days <= as.integer(today)]
  if (!length(days)) return(c(current = 0L, longest = 0L))
  runs <- tabulate(cumsum(c(TRUE, diff(days) != 1L)))
  current <- if (max(days) >= as.integer(today) - 1L) utils::tail(runs, 1L) else 0L
  c(current = current, longest = max(runs))
}
