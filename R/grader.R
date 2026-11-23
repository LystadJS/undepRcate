# This file runs in Shiny. It never parses or evaluates a submission.
assessment_failure <- function(status, detail) {
  list(status = status,
       checks = data.frame(label = "Assessment", passed = FALSE, detail = detail),
       warnings = character(), output = character(), elapsed = 0)
}

assess_answer <- function(exercise, code, timeout = getOption("undepRcate.timeout", getOption("rreps.timeout", 15))) {
  started <- proc.time()[["elapsed"]]
  scratch <- tempfile("undepRcate-grade-")
  if (!dir.create(scratch)) return(assessment_failure("infrastructure_error", "Cannot create working directory."))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  console <- file.path(scratch, "console.log")
  messages <- file.path(scratch, "messages.log")

  review <- tryCatch({
    stopifnot(is.character(code), length(code) == 1L, !is.na(code),
              is.numeric(timeout), length(timeout) == 1L, is.finite(timeout), timeout > 0)
    worker <- normalizePath(file.path(getOption("undepRcate.root", "."), "R", "worker.R"), mustWork = TRUE)
    result <- callr::r(function(worker, specification, submission, message_path) {
      # Start capturing after R initializes so locale/startup notices stay out of feedback.
      stream <- file(message_path, open = "wt")
      sink(stream, type = "message")
      on.exit({ sink(type = "message"); close(stream) }, add = TRUE)
      engine <- new.env(parent = globalenv())
      sys.source(worker, envir = engine)
      engine$grade_submission(specification, submission)
    }, args = list(worker, exercise[c("category", "solution", "fixtures", "contract", "seed")], code, messages),
    wd = scratch, timeout = timeout, spinner = FALSE, stdout = console,
    libpath = .libPaths(), system_profile = FALSE, user_profile = FALSE)

    valid_checks <- is.data.frame(result$checks) && nrow(result$checks) > 0L &&
      all(c("label", "passed", "detail") %in% names(result$checks)) &&
      is.logical(result$checks$passed) && !anyNA(result$checks$passed)
    if (!valid_checks || !result$status %in% c("pass", "incorrect", "syntax_error", "runtime_error") ||
        (identical(result$status, "pass") != all(result$checks$passed))) {
      stop("Invalid result from grading worker.")
    }
    result
  }, error = function(e) {
    if (inherits(e, c("callr_timeout_error", "system_command_timeout_error"))) {
      assessment_failure("timeout", sprintf("Execution exceeded %s seconds.", timeout))
    } else {
      assessment_failure("infrastructure_error", conditionMessage(e))
    }
  })
  # Limit the amount of student console output returned to the browser.
  read_log <- function(path) {
    if (!file.exists(path)) return(character())
    substr(readLines(path, n = 30L, warn = FALSE), 1L, 400L)
  }
  review$output <- c(read_log(console), read_log(messages))
  review$elapsed <- unname(proc.time()[["elapsed"]] - started)
  review
}
