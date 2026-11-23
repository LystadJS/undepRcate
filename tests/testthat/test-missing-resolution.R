testthat::test_that("resolution drills grade plausible shortcuts against the declared contracts", {
  statuses <- callr::r(function(root) {
    source(file.path(root, "R", "exercises.R"), local = TRUE)
    source(file.path(root, "R", "exercises-missing-resolution.R"), local = TRUE)
    source(file.path(root, "R", "worker.R"), local = TRUE)
    # Each edit loses a distinct substantive safeguard while preserving the
    # rest of the reference, including its output column names.
    substitutions <- list(
      missing_source_reconcile = c("if_else(disputed, NA_integer_, field)", "field"),
      missing_identity = c("!applicable ~ NA_integer_", "!applicable ~ 0L"),
      missing_eligible = c("c(score, exposure, baseline)", "c(score, exposure, baseline, notes)"),
      missing_carry = c("group_by(device, shift, run)", "group_by(device, shift)"),
      missing_observation_weight = c("1 / p_observed", "p_observed"),
      missing_delta_grid = c("else max(shift)", "else min(shift)"),
      missing_binary_bounds = c("lower[cohort == 'B'] - upper[cohort == 'A']",
                                "lower[cohort == 'B'] - lower[cohort == 'A']"),
      missing_compare_reports = c("quantile(paired$change, .025, names = FALSE)",
                                  "quantile(paired$weighted, .025) - quantile(paired$respondents, .025)"),
      missing_train_only = c("known <- train |>", "known <- bind_rows(train, heldout) |>"),
      missing_mask_validation = c("group_by(id) |> filter(n_distinct(method) == 2L, all(!is.na(prediction) & !is.na(lower) & !is.na(upper)))",
                                  "group_by(id) |> filter(!is.na(prediction) & !is.na(lower) & !is.na(upper))")
    )
    vapply(names(substitutions), function(id) {
      ex <- make_exercise(id, 282, 3L)
      edits <- substitutions[[id]]
      wrong <- sub(edits[1], edits[2], ex$solution, fixed = TRUE)
      grade_submission(ex, wrong)$status
    }, "")
  }, args = list(root = getOption("undepRcate.root")), libpath = .libPaths())
  testthat::expect_true(all(statuses == "incorrect"), info = paste(names(statuses), statuses))
})

testthat::test_that("preprocessing preserves missingness signals and validation scores only synthetic holdouts", {
  statuses <- callr::r(function(root) {
    source(file.path(root, "R", "exercises.R"), local = TRUE)
    source(file.path(root, "R", "exercises-missing-resolution.R"), local = TRUE)
    source(file.path(root, "R", "worker.R"), local = TRUE)
    train <- make_exercise("missing_train_only", 729, 3L)
    validation <- make_exercise("missing_mask_validation", 729, 3L)
    c(
      missing_fallback = grade_submission(train, sub("coalesce(value, site_center, center)",
        "coalesce(value, site_center)", train$solution, fixed = TRUE))$status,
      indicator_after_fill = grade_submission(train, sub("was_missing = is.na(value)",
        "was_missing = is.na(coalesce(value, site_center, center))", train$solution, fixed = TRUE))$status,
      scores_visible = grade_submission(validation, sub("filter(masked, !is.na(truth))",
        "filter(!is.na(truth))", validation$solution, fixed = TRUE))$status,
      absent_method = grade_submission(validation, sub("n_distinct(method) == 2L, ",
        "", validation$solution, fixed = TRUE))$status,
      excludes_boundary = grade_submission(validation, sub("lower <= truth & truth <= upper",
        "lower < truth & truth < upper", validation$solution, fixed = TRUE))$status
    )
  }, args = list(root = getOption("undepRcate.root")), libpath = .libPaths())
  testthat::expect_true(all(statuses == "incorrect"), info = paste(names(statuses), statuses))
})

testthat::test_that("both reported bootstrap estimates use the same resampled respondent IDs with multiplicity", {
  respondents <- data.frame(score = c(2, 5, 10, 30), p_observed = c(.5, .5, .25, .25))
  # Both strata have two respondents, resampled with replacement here.
  computed <- resolution_report_estimates(respondents, c(2L, 2L, 3L, 4L))
  testthat::expect_equal(computed, c(respondents = 12.5, weighted = 15))
  ex <- make_exercise("missing_compare_reports", 84L, 3L)
  for (fixture in ex$fixtures) {
    per_draw <- table(fixture$replicates$draw, fixture$replicates$method)
    testthat::expect_true(all(per_draw == 1L) && ncol(per_draw) == 2L)
  }
})

testthat::test_that("sensitivity and eligibility fixtures include non-estimable cases", {
  tipping <- make_exercise("missing_delta_grid", 491L, 3L)
  compute_tipping <- function(f) {
    candidates <- f$scenarios$shift
    qualifies <- vapply(candidates, function(shift) {
      scores <- f$df$score
      missing <- is.na(scores)
      scores[missing] <- f$df$prediction[missing] + shift
      mean(scores) < f$threshold
    }, logical(1))
    if (any(qualifies)) max(candidates[qualifies]) else NA_real_
  }
  answers <- vapply(tipping$fixtures, compute_tipping, numeric(1))
  testthat::expect_lt(answers[1], 0)
  testthat::expect_true(is.na(answers[2]))
  testthat::expect_equal(answers[3], 0)
  attrition <- make_exercise("missing_eligible", 822L, 3L)
  excluded_site <- subset(attrition$fixtures[[2L]]$df, eligible & site == "North")
  testthat::expect_true(all(is.na(excluded_site$baseline)) && any(!is.na(excluded_site$score)))
  unmeasured_site <- subset(attrition$fixtures[[3L]]$df, eligible & site == "West")
  testthat::expect_true(all(is.na(unmeasured_site$score)))
  bound_data <- make_exercise("missing_binary_bounds", 923L, 3L)$fixtures[[2L]]$df
  testthat::expect_true(all(is.na(bound_data$event[bound_data$cohort == "B"])))
})
