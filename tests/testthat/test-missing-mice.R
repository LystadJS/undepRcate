testthat::test_that("typed methods, predictor direction and structural masks have counterexamples", {
  methods <- make_exercise("mice_methods", 150L, 3L)
  sample <- methods$inputs$df
  testthat::expect_true(is.ordered(sample$rating))
  testthat::expect_false(is.ordered(sample$channel))
  testthat::expect_equal(assess_answer(methods, "mice::make.method(df)")$status, "incorrect")

  pred <- make_exercise("mice_predictors", 151L, 3L)
  testthat::expect_equal(assess_answer(pred, paste0("t({\n", pred$solution, "\n})"))$status, "incorrect")
  # The retrospective study's observed interaction belongs in the exposure model.
  testthat::expect_equal(pred$inputs$df$baseline_aux, with(pred$inputs$df, baseline * auxiliary))
  omitted <- paste0("pred <- {\n", pred$solution, '\n}\npred[, "baseline_aux"] <- 0\npred')
  testthat::expect_equal(assess_answer(pred, omitted)$status, "incorrect")

  mask <- make_exercise("mice_where", 152L, 3L)
  testthat::expect_true(all(vapply(mask$fixtures, function(f) {
    any(!f$df$eligible) && any(f$df$eligible & f$df$check_observed)
  }, logical(1))))
  testthat::expect_equal(assess_answer(mask, "mice::make.where(df)")$status, "incorrect")
})

testthat::test_that("seeded chained equations preserve structural gaps and exclude future predictors", {
  ex <- make_exercise("mice_fit", 340L, 3L)
  inputs <- ex$inputs
  testthat::expect_true(all(inputs$predictors[, "post_visit"] == 0))
  testthat::expect_equal(sum(inputs$predictors), 2)
  incomplete <- names(inputs$df)[vapply(inputs$df, anyNA, logical(1))]
  testthat::expect_true(all(!inputs$where[!inputs$df$eligible, incomplete]))
  testthat::expect_true(any(inputs$where[inputs$df$eligible, incomplete]))
  testthat::expect_equal(assess_answer(ex, ex$solution)$status, "pass")
  fill_every_gap <- sub(", where = where", "", ex$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(ex, fill_every_gap)$status, "incorrect")
})

testthat::test_that("actual mids diagnostics use within-chain changes and original cell status", {
  chains <- make_exercise("mice_chains", 42L, 3L)
  testthat::expect_s3_class(chains$inputs$imp, "mids")
  testthat::expect_identical(chains, make_exercise("mice_chains", 42L, 3L))
  testthat::expect_equal(dim(chains$inputs$imp$chainMean)[2:3], c(4L, 3L))
  wrong <- sub(", .by = chain", "", chains$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(chains, wrong)$status, "incorrect")

  compare <- make_exercise("mice_distributions", 610L, 2L)
  swapped <- sub('"observed", "imputed"', '"imputed", "observed"', compare$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(compare, swapped)$status, "incorrect")
})

testthat::test_that("model pooling accepts a separate fit per completion and rejects within-only uncertainty", {
  ex <- make_exercise("mice_pool", 64L, 3L)
  # The prompt fixes the formula, while the implementation can extract and map
  # over completed data frames instead of dispatching with.mids().
  with_line <- strsplit(ex$solution, "\n", fixed = TRUE)[[1]][1]
  formula <- sub("^fits <- with\\(imp, lm\\((.+)\\)\\)$", "\\1", with_line)
  alternate <- sub(with_line,
    sprintf('fits <- mice::complete(imp, "all") |> map(\\(dataset) lm(%s, data = dataset))', formula),
    ex$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(ex, alternate)$status, "pass")
  wrong <- sub("total = t, std.error = sqrt(t)", "total = ubar, std.error = sqrt(ubar)",
               ex$solution, fixed = TRUE)
  testthat::expect_equal(assess_answer(ex, wrong)$status, "incorrect")

  single <- make_exercise("mice_pool", 64L, 1L)
  only_one <- sub("pooled <- mice::pool(fits)", "pooled <- fits$analyses[[1]]",
                  single$solution, fixed = TRUE)
  only_one <- sub("summary(pooled)", "broom::tidy(pooled)", only_one, fixed = TRUE)
  testthat::expect_equal(assess_answer(single, only_one)$status, "incorrect")
})
