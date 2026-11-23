testthat::test_that("the library covers every category and all templates have runnable variants", {
  catalog <- exercise_catalog()
  testthat::expect_gte(nrow(catalog), 24L)
  required <- c("R fundamentals", "Filtering and selection", "Variable construction",
                "Grouped operations", "Joining datasets", "Reshaping and missing values",
                "Visualization", "Statistical programming", "Missing Data")
  testthat::expect_true(all(required %in% catalog$category))
  testthat::expect_identical(anyDuplicated(catalog$id), 0L)
  testthat::expect_true(all(vapply(split(catalog$difficulty, catalog$category)[required],
                                   function(x) all(1:3 %in% x), logical(1))))
  for (id in catalog$id) {
    instructions <- character(3L)
    solutions <- character(3L)
    for (level in 1:3) {
      ex <- make_exercise(id, 87123L, level)
      instructions[level] <- ex$instructions
      solutions[level] <- ex$solution
      testthat::expect_identical(ex, make_exercise(id, 87123L, level))
      testthat::expect_identical(ex$inputs, ex$fixtures[[1]])
      testthat::expect_true(all(vapply(ex$fixtures, function(f) identical(names(f), names(ex$inputs)), logical(1))))
      testthat::expect_false(identical(ex$fixtures[[2]], ex$fixtures[[1]]))
      testthat::expect_false(identical(ex$fixtures[[2]], ex$fixtures[[3]]))
      testthat::expect_length(ex$hints, 4L)
      testthat::expect_true(all(nzchar(ex$hints)) && nzchar(ex$instructions) && nzchar(ex$explanation))
      testthat::expect_true(grepl(ex$solution, ex$hints[4L], fixed = TRUE))
      testthat::expect_true(length(parse(text = ex$solution)) > 0)
    }
    testthat::expect_length(unique(instructions), 3L)
    testthat::expect_length(unique(solutions), 3L)
  }
})

testthat::test_that("a 20-rep grouped set varies data and tasks without changing the skill or global RNG", {
  seeds <- 487:506
  set.seed(41)
  baseline <- .Random.seed
  reps <- lapply(seeds, function(seed) make_exercise("grouped_summary", seed))
  testthat::expect_identical(.Random.seed, baseline)
  testthat::expect_true(all(vapply(reps, function(ex) ex$id == "grouped_summary", logical(1))))
  testthat::expect_length(unique(lapply(reps, function(ex) ex$inputs)), 20L)
  testthat::expect_gte(length(unique(vapply(reps, function(ex) ex$instructions, ""))), 10L)
  testthat::expect_false(identical(make_exercise("grouped_summary", 1, 1)$instructions,
                                   make_exercise("grouped_summary", 1, 3)$instructions))
  testthat::expect_error(make_exercise("unknown", 12), "Unknown")
  testthat::expect_error(make_exercise("grouped_summary", Inf), "Seed")
  testthat::expect_error(make_exercise("grouped_summary", 1L, 4L), "Level")
})

testthat::test_that("every reference passes its own checker on displayed and hidden fixtures", {
  # A single test-only child runs this curriculum audit. Interactive answers
  # always use assess_answer(), which creates a new process for each submission.
  results <- callr::r(function(root) {
    for (f in Sys.glob(file.path(root, "R", "exercises*.R"))) source(f, local = TRUE)
    source(file.path(root, "R", "worker.R"), local = TRUE)
    ids <- exercise_catalog()$id
    variants <- expand.grid(id = ids, level = 1:3, stringsAsFactors = FALSE)
    results <- vapply(seq_len(nrow(variants)), function(i) {
      ex <- make_exercise(variants$id[i], 771L, variants$level[i])
      graded <- grade_submission(ex, ex$solution)
      if (identical(graded$status, "pass")) return("pass")
      paste(graded$status, paste(graded$checks$detail[!graded$checks$passed], collapse = " / "))
    }, "")
    setNames(results, paste(variants$id, variants$level))
  }, args = list(root = getOption("undepRcate.root")), libpath = .libPaths())
  testthat::expect_true(all(results == "pass"), info = paste(names(results)[results != "pass"],
                                                              results[results != "pass"]))
})

testthat::test_that("empty groups, join keys and missing pivot entries have real counterexamples", {
  empty <- make_exercise("count_empty_groups", 44L)$inputs$df
  factor_col <- names(empty)[vapply(empty, is.factor, logical(1))]
  testthat::expect_true(any(table(empty[[factor_col]]) == 0))
  join <- make_exercise("join_left", 52L, 3L)$inputs
  testthat::expect_false(identical(names(join$df)[1], names(join$lookup)[1]))
  testthat::expect_true(any(!join$df[[1]] %in% join$lookup[[1]]))
  testthat::expect_true(anyDuplicated(join$df[[1]]) > 0L)
  wide <- make_exercise("pivot_wide", 54L, 2L)$fixtures[[3L]]$df
  testthat::expect_true(any(table(wide[[1]], wide[[2]]) == 0))
  testthat::expect_true(anyNA(wide[[3L]]))
})
