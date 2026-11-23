testthat::test_that("the Missing Data menu covers every drill once in learning order", {
  pool <- dplyr::filter(exercise_catalog(), category == "Missing Data")
  ids <- unname(unlist(missing_data_stages))
  testthat::expect_setequal(ids, pool$id)
  testthat::expect_identical(anyDuplicated(ids), 0L)
  testthat::expect_identical(ordered_drills(pool)$id, ids)
  choices <- drill_choices(ordered_drills(pool), "Missing Data")
  testthat::expect_identical(names(choices)[seq_along(missing_data_stages)], names(missing_data_stages))
  testthat::expect_identical(unname(unlist(choices)), c(ids, "category/Missing Data"))
})

testthat::test_that("scheduled follow-up and target coverage use eligible records", {
  ex <- make_exercise("md_followup", 917L, 3L)
  # The unscheduled score must not turn this person's trailing gap into an
  # intermittent gap. A person with no observed visits is a separate case.
  measure <- setdiff(names(ex$inputs$visits), c("visit", "due", "id"))
  visits <- tibble::tibble(id = c(1L, 1L, 1L, 2L), visit = c(4L, 1L, 2L, 1L),
                           due = c(FALSE, TRUE, TRUE, TRUE))
  visits[[measure]] <- c(7, 2, NA, NA)
  ex$fixtures <- rep(list(list(visits = visits)), 3L)
  answer <- "tibble::tibble(id = 1:2, intermittent = c(0,0), trailing = c(1,0), never_observed = c(FALSE,TRUE))"
  testthat::expect_identical(assess_answer(ex, answer)$status, "pass")
  testthat::expect_identical(assess_answer(ex, sub("c(1,0)", "c(0,0)", answer, fixed=TRUE))$status, "incorrect")
  ex <- make_exercise("md_overlap", 167L, 3L)
  data <- tibble::tibble(site = c("A", "B"), risk = "low", person = 1L)
  data[[setdiff(names(ex$inputs$data), names(data))]] <- c(8, NA_real_)
  target <- tibble::tibble(site = c("A", "B", "C"), risk = "low", target_count = c(6,1,3))
  ex$fixtures <- rep(list(list(data = data, target = target)), 3L)
  testthat::expect_identical(assess_answer(ex, "tibble::tibble(unsupported_share = 0.4)")$status, "pass")
})

testthat::test_that("the renamed checkout repairs only generated paths in an old saved context", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  project <- file.path(root, "undepRcate")
  dir.create(project)
  withr::local_dir(project)
  withr::local_options(list(undepRcate.root = project))
  path <- "practice/example"
  dir.create(path, recursive = TRUE)
  input_path <- file.path(path, "inputs.rds")
  original <- list(df = data.frame(score = c(2, NA_real_)))
  saveRDS(original, input_path)
  context <- file.path(path, "context.R")
  old <- file.path(root, "RReps")
  writeLines(c("# my note", sprintf('.libPaths(c("%s/.local-library", .libPaths()))', old),
               sprintf('list2env(readRDS("%s/%s"), envir = .GlobalEnv)', old, input_path)), context)
  testthat::expect_true(relocate_context(context))
  testthat::expect_false(relocate_context(context))
  testthat::expect_identical(readLines(context)[1], "# my note")
  loaded <- callr::r(function(script) { source(script); df }, args=list(context), wd=project)
  testthat::expect_identical(loaded, original$df)
})

testthat::test_that("a broken strict storage specification fails closed", {
  ex <- make_exercise("grouped_summary", 31L)
  ex$contract$storage <- c(no_such_column = "integer")
  testthat::expect_identical(assess_answer(ex, ex$solution)$status, "infrastructure_error")
})
