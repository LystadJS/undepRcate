testthat::test_that("every built-in drill teaches its worked example at each level", {
  for (id in exercise_catalog()$id) {
    for (level in 1:3) {
      ex <- make_exercise(id, 347L, level, preview_only = TRUE)
      notes <- lesson_notes(ex)
      valid <- length(notes$intuition) == 1L && nzchar(notes$intuition) &&
        length(notes$steps) >= 2L && all(nzchar(notes$steps)) &&
        length(notes$math) >= 1L && all(nzchar(notes$math)) &&
        length(notes$symbols) > 0L && all(vapply(notes$symbols, function(row)
          is.character(row) && length(row) == 5L && !anyNA(row) && all(nzchar(row)), logical(1)))
      testthat::expect_true(valid, info = paste(id, level))
    }
  }
})

testthat::test_that("worked answers show the result of executing the displayed code", {
  ex <- make_exercise("conditional_function", 22L, 3L, preview_only = TRUE)
  calls <- run_worked_example(ex)
  testthat::expect_true(all(c("Arguments", "Returns") %in% names(calls)))
  testthat::expect_true(any(calls$Returns == '"unknown"'))
  testthat::expect_true(any(calls$Returns == '"middle"'))
  ex <- make_exercise("scatter_mapping", 331L, preview_only = TRUE)
  plot <- as.character(run_worked_example(ex))
  testthat::expect_match(plot, "data:image/png;base64,", fixed = TRUE)
  ex <- make_exercise("mice_complete", 408L, preview_only = TRUE)
  table <- run_worked_example(ex)
  testthat::expect_equal(nrow(table), nrow(ex$inputs$imp$data))
  testthat::expect_false(anyNA(table))
  testthat::expect_match(as.character(guided_preview(ex$inputs$imp, "imp")),
    sprintf("Show all %d rows", nrow(ex$inputs$imp$data)), fixed = TRUE)
})

testthat::test_that("Guided resumes the lesson or the current practice repetition", {
  db <- tempfile(fileext = ".sqlite")
  withr::local_options(list(undepRcate.db = db, undepRcate.practice = tempfile()))
  saved <- new.env(parent = emptyenv())
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    session$setInputs(mode = "Guided", skill = "grouped_summary", ordering = "Randomized", target = "5")
    session$setInputs(start = 1L)
    s <- current()
    testthat::expect_identical(s$ordering, "Sequential")
    testthat::expect_equal(s$lesson_pending, 1L)
    testthat::expect_identical(rep_token(s), "")
    testthat::expect_null(item())
    testthat::expect_identical(editor_message(s, item())$guide, "")
    for (label in c("Worked example", "Walk through the example", "Worked answer", "What the code returns",
                    "The idea in notation", "Conditioning", "Finish lesson and start practice")) {
      testthat::expect_match(output$guided_lesson$html, label, fixed = TRUE)
    }
    testthat::expect_false(grepl("data-start", output$session_heading$html, fixed = TRUE))
    saved$input <- worked()$ex$inputs
    saved$seed <- worked()$ex$seed
    saved$stat <- sub(".*Compute the (mean|median|sum) of .*", "\\1", worked()$ex$instructions)
    saved$token <- lesson_token(s)
    session$setInputs(submission = list(token = "", source = "In-app", code = "1"))
    session$setInputs(draft_event = list(token = "", code = "delayed"), hint = 1L,
                      finish_lesson = "lesson-from-another-session")
    testthat::expect_identical(current(), s)
    testthat::expect_equal(nrow(attempt_history(con)), 0L)
  })
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    testthat::expect_identical(worked()$ex$inputs, saved$input)
    withr::with_options(list(undepRcate.timeout = 0.01), {
      session$setInputs(retry_example = saved$token)
      testthat::expect_match(output$guided_lesson$html, "Run the worked answer again", fixed = TRUE)
      testthat::expect_match(output$guided_lesson$html, "Finish lesson and start practice", fixed = TRUE)
    })
    session$setInputs(retry_example = saved$token)
    testthat::expect_false(inherits(worked()$answer, "error"))
    session$setInputs(finish_lesson = saved$token)
    s <- current()
    testthat::expect_equal(c(s$lesson_pending, s$position, s$target, s$hints), c(0, 1, 5, 0))
    testthat::expect_identical(s$draft, "")
    editor <- editor_message(s, item())
    testthat::expect_identical(editor$guide, item()$solution)
    testthat::expect_identical(editor$value, "")
    session$setInputs(submission = list(token = editor$token, source = "In-app", code = ""))
    testthat::expect_equal(nrow(attempt_history(con)), 0L)
    testthat::expect_false(identical(item()$seed, saved$seed))
    testthat::expect_false(identical(item()$inputs, saved$input))
    testthat::expect_match(item()$instructions, paste("Compute the", saved$stat, "of"), fixed = TRUE)
    testthat::expect_equal(nrow(attempt_history(con)), 0L)
    session$setInputs(submission = list(token = rep_token(s), source = "In-app", code = item()$solution))
    testthat::expect_equal(current()$passed, 1L)
    testthat::expect_identical(editor_message(current(), item())$guide, "")
    session$setInputs(advance = 1L)
    testthat::expect_equal(current()$position, 2L)
    testthat::expect_identical(current()$seed, new_seed(s))
    testthat::expect_equal(current()$hints, 0L)
    testthat::expect_identical(editor_message(current(), item())$guide, item()$solution)
    saved$guide <- item()$solution
    saved$practice <- rep_token(current())
    session$setInputs(draft_event = list(token = saved$practice, code = "df |>"))
  })
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    testthat::expect_identical(rep_token(current()), saved$practice)
    testthat::expect_identical(current()$draft, "df |>")
    editor <- editor_message(current(), item())
    testthat::expect_identical(editor$value, "df |>")
    testthat::expect_identical(editor$guide, saved$guide)
    testthat::expect_equal(current()$lesson_pending, 0L)
    testthat::expect_equal(nrow(attempt_history(con)), 1L)
    session$setInputs(source_mode = "Positron")
    testthat::expect_match(output$file_info$html, "Use this typing guide", fixed = TRUE)
    # A file opened earlier may be empty or may hold the learner's draft.
    # Its reference answer lives only in the browser.
    testthat::expect_true(read_practice_file(current()) %in% c("", "df |>"))
    writeLines(item()$solution, file.path(practice_folder(current()), "answer.R"))
    session$setInputs(submission = list(token = editor$token, source = "Positron", code = ""))
    testthat::expect_equal(current()$passed, 1L)
    session$setInputs(advance = 1L)
    testthat::expect_equal(current()$position, 3L)
    testthat::expect_identical(editor_message(current(), item())$guide, "")
    testthat::expect_identical(current()$draft, "")
    testthat::expect_false(grepl("Use this typing guide", output$file_info$html, fixed = TRUE))
    session$setInputs(submission = list(token = rep_token(current()), source = "In-app", code = item()$solution))
    testthat::expect_equal(attempt_history(con)$guide_shown, c(1L, 1L, 0L))
    labels <- gregexpr("Typing guide shown", output$history_submissions$html, fixed = TRUE)
    testthat::expect_length(regmatches(output$history_submissions$html, labels)[[1L]], 2L)
  })
})

testthat::test_that("the typing guide is confined to active Guided practice", {
  practice <- list(lesson_pending = 0L, position = 1L, passed = 0L)
  for (mode in c("Recall", "Speed", "Mastery")) {
    testthat::expect_false(has_typing_guide(c(practice, list(mode = mode))))
  }
  testthat::expect_identical(editor_message(NULL, NULL)$guide, "")
})
