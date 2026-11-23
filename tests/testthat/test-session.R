# An alternative grouped summary expressed with per-operation grouping. Read
# the task and displayed schema as a learner would, including the advanced levels.
grouped_answer <- function(ex) {
  cols <- names(ex$inputs$df)
  groups <- if (ex$level > 1L) cols[2:3] else cols[2]
  stat <- sub(".*Compute the (mean|median|sum) of .*", "\\1", ex$instructions)
  measure <- cols[4]
  sprintf("df |> summarise(%s_%s = %s(%s, na.rm = TRUE)%s, .by = c(%s))",
          stat, measure, stat, measure, if (ex$level == 3L) ", records = n()" else "",
          paste(groups, collapse = ", "))
}

testthat::test_that("twenty real grouped submissions survive a restart and schedule a next-day review", {
  sandbox <- withr::local_tempdir()
  db <- file.path(sandbox, "progress.sqlite")
  withr::local_options(list(undepRcate.db = db, undepRcate.practice = file.path(sandbox, "practice")))
  checkpoint <- new.env(parent = emptyenv())

  shiny::testServer(undepRcate_server, {
    session$flushReact()
    session$setInputs(category = "Grouped operations", skill = "grouped_summary",
                      mode = "Recall", ordering = "Sequential", target = "20")
    session$setInputs(start = 1L)
    testthat::expect_equal(current()$target, 20L)
    testthat::expect_match(output$preview$html, "columns", fixed = TRUE)
    token <- rep_token(current())
    session$setInputs(submission = list(token = token, source = "In-app", code = "df"))
    testthat::expect_identical(result()$review$status, "incorrect")
    testthat::expect_equal(current()$hints, 0L)
    session$setInputs(hint = 1L)
    testthat::expect_match(output$hint_text$html, "Concept")
    testthat::expect_false(grepl("Worked answer", output$hint_text$html, fixed = TRUE))
    checkpoint$token <- token
    checkpoint$inputs <- item()$inputs
    checkpoint$first_code <- grouped_answer(item())
    session$setInputs(draft_event = list(token = token, code = checkpoint$first_code))
    testthat::expect_identical(current()$draft, checkpoint$first_code)
  })

  shiny::testServer(undepRcate_server, {
    session$flushReact()
    testthat::expect_identical(rep_token(current()), checkpoint$token)
    testthat::expect_identical(item()$inputs, checkpoint$inputs)
    testthat::expect_identical(current()$draft, checkpoint$first_code)
    testthat::expect_equal(current()$hints, 1L)
    for (position in seq_len(20L)) {
      s <- current()
      ex <- item()
      testthat::expect_equal(s$position, position)
      testthat::expect_identical(ex$id, "grouped_summary")
      answer <- grouped_answer(ex)
      if (position == 2L) {
        session$setInputs(source_mode = "Positron")
        testthat::expect_match(output$file_info$html, "answer.R", fixed = TRUE)
        path <- file.path(practice_folder(s), "answer.R")
        testthat::expect_identical(readRDS(file.path(dirname(path), "inputs.rds")), ex$inputs)
        writeLines(answer, path)
        # A conflicting browser value proves the submitted bytes came from disk.
        session$setInputs(submission = list(token = rep_token(s), source = "Positron", code = "stop('wrong source')"))
        session$setInputs(source_mode = "In-app")
      } else {
        session$setInputs(submission = list(token = rep_token(s), source = "In-app", code = answer))
      }
      testthat::expect_identical(result()$review$status, "pass")
      if (position == 2L) testthat::expect_match(output$feedback$html, "Passed", fixed = TRUE)
      testthat::expect_true(all(result()$review$checks$passed))
      testthat::expect_true(any(startsWith(result()$review$checks$label, "Hidden 2")))
      testthat::expect_equal(current()$passed, 1L)
      session$setInputs(advance = position)
    }
    testthat::expect_null(current())
    testthat::expect_match(finished(), "20 repetitions passed across 21 submissions", fixed = TRUE)
    history <- attempt_history(con)
    testthat::expect_equal(length(unique(history$seed)), 20L)
    testthat::expect_true(all(1:3 %in% history$level))
    testthat::expect_equal(history$hints[history$position == 1L], c(0L, 1L))
    testthat::expect_true(all(history$seconds >= 0) && all(history$execution_seconds > 0))
    testthat::expect_equal(skill_progress(con)$unseen_successes, 19L)
    testthat::expect_match(output$home_review_queue$html, "up to date", fixed = TRUE)
    session$setInputs(history_category = "Grouped operations", history_template = "grouped_summary",
                      history_mode = "Recall", history_period = "All dates")
    testthat::expect_match(output$history_by_skill$html, "95%", fixed = TRUE)
    session$setInputs(history_mode = "Speed")
    testthat::expect_match(output$history_graphs$html, "No submissions match", fixed = TRUE)
  })

  # Advance the calendar for the test database only. Preserve ordering and
  # elapsed times by shifting all attempts together to noon on the previous day.
  yesterday <- as.Date(format(Sys.time(), "%Y-%m-%d")) - 1L
  con <- db_open(db)
  first_attempt <- min(attempt_history(con)$attempted_at)
  shift <- as.numeric(as.POSIXct(paste(yesterday, "12:00:00"), tz = "")) - first_attempt
  DBI::dbExecute(con, "UPDATE attempts SET attempted_at=attempted_at+?, practice_date=?",
                 params = list(shift, as.character(yesterday)))
  db_close(con)
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    testthat::expect_null(current())
    testthat::expect_equal(nrow(attempt_history(con)), 21L)
    testthat::expect_match(output$home_review_queue$html, 'data-template="grouped_summary"', fixed = TRUE)
    session$setInputs(review_template = "grouped_summary")
    testthat::expect_identical(current()$focus, "grouped_summary")
    testthat::expect_equal(current()$target, 5L)
    testthat::expect_equal(current()$position, 1L)
  })
})
