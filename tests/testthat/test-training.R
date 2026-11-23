testthat::test_that("a mixed sequence stays in category and unlocks its difficulty tiers", {
  catalog <- exercise_catalog()
  group <- catalog[catalog$category == "Grouped operations", ]
  focus <- "category/Grouped operations"
  empty <- data.frame(template_id = character(), unseen_successes = integer())
  sequence_for <- function(progress, ordering) {
    vapply(seq_len(36L), function(i)
      next_drill(focus, catalog, progress, ordering, i, i + 1000L), "")
  }
  first <- sequence_for(empty, "Sequential")
  testthat::expect_setequal(unique(first), group$id[group$difficulty == 1L])
  earned <- data.frame(template_id = "grouped_summary", unseen_successes = 8L)
  unlocked <- sequence_for(earned, "Sequential")
  testthat::expect_setequal(unique(unlocked), group$id)
  randomized <- sequence_for(earned, "Randomized")
  testthat::expect_identical(randomized, sequence_for(earned, "Randomized"))
  testthat::expect_true(all(randomized %in% group$id))
  testthat::expect_false(identical(randomized, unlocked))
  testthat::expect_identical(next_drill("grouped_summary", catalog, earned, "Randomized", 99, 37), "grouped_summary")
  testthat::expect_identical(new_seed(list(ordering = "Sequential", seed = 1000000000L)), 0L)
})

testthat::test_that("editor preferences are local and restored", {
  withr::local_options(list(undepRcate.db = tempfile(fileext = ".sqlite")))
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    session$setInputs(daily_goal = 12, speed_goal = 45, show_timer = FALSE,
                      accent = "blue", disable_suggestions = FALSE, save_prefs = 1)
    testthat::expect_identical(get_setting(con, "disable_suggestions"), "no")
    testthat::expect_identical(get_setting(con, "daily_goal"), "12")
  })
  shiny::testServer(undepRcate_server, {
    session$flushReact()
    testthat::expect_identical(pref("accent", "mint"), "blue")
    testthat::expect_identical(pref("disable_suggestions", "yes"), "no")
    session$setInputs(daily_goal = 12, speed_goal = 45, show_timer = FALSE,
                      accent = "blue", disable_suggestions = TRUE, save_prefs = 1)
    testthat::expect_identical(get_setting(con, "disable_suggestions"), "yes")
  })
})
