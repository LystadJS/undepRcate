# The first three drills construct separately inspectable pieces of a mice plan.
# Execution, extraction, diagnostics and analysis each practice one subsequent step.
mice_method_exercise <- function(ctx, level) {
  methods <- "mice::make.method(df)"
  instruction <- sprintf(
    "Use mice defaults to choose imputation methods for the incomplete numeric field %s and binary factor enrolled. Return a named character vector in that order.", ctx$x)
  solution <- sprintf('df |> select(%s, enrolled) |> mice::make.method()', ctx$x)
  if (level == 2L) {
    instruction <- paste("Return the complete named mice method vector in df column order.",
      "Use predictive mean matching for numeric measurements, logistic regression for binary factors,",
      "multinomial regression for unordered factors with three or more levels, and proportional odds for ordered factors.",
      "Leave complete variables unimputed.")
    solution <- methods
  } else if (level == 3L) {
    instruction <- paste("Return a named method vector in df column order for this analysis.",
      "For incomplete measurements use normal linear regression for numeric fields, logreg for binary factors,",
      "polyreg for unordered multilevel factors and polr for ordered multilevel factors.",
      "An incomplete field named archived is outside this analysis and must be skipped. Leave complete fields unimputed.")
    solution <- paste('mice::make.method(df, defaultMethod = c("norm", "logreg", "polyreg", "polr")) |>',
      '  tibble::enframe(name = "field", value = "method") |>',
      '  mutate(method = if_else(field == "archived", "", method)) |> tibble::deframe()', sep = "\n")
  }
  exercise_plan(instruction, solution,
    "Check the variable's recorded measurement type, factor levels and whether it needs any imputations.",
    "mice::make.method() generates the named vector. Its defaultMethod entries follow measurement type.",
    if (level == 3L) 'method <- mice::make.method(df, defaultMethod = c("norm", ...))' else "method <- mice::make.method(df)",
    paste("An empty method skips a field. Match each imputed variable's model to its type.",
      "For a quantity defined from other measurements, recompute it from their completed values or use a passive mice formula to maintain the defining relationship."),
    fixture = function(ctx, variant) {
      size <- sample(28:40, 1L)
      df <- data.frame(id = sample.int(1000L, size), measure = stats::rnorm(size, mean = 30, sd = 4),
                       enrolled = factor(rep(c("yes", "no"), length.out = size)))
      names(df)[2L] <- ctx$x
      if (level > 1L) {
        df$channel <- factor(rep(c("mail", "phone", "web"), length.out = size))
        df$rating <- ordered(rep(c("low", "medium", "high"), length.out = size),
                             levels = c("low", "medium", "high"))
      }
      if (level == 3L) df$archived <- stats::rnorm(size)
      for (name in setdiff(names(df), "id")) df[[name]][sample.int(size, 3L)] <- NA
      list(df = df[sample(names(df))])
    })
}

mice_predictor_exercise <- function(ctx, level) {
  instruction <- sprintf(
    "Start with the default mice predictor matrix for df and exclude %s as a predictor in every model. Keep all other entries. Return the named numeric matrix.", ctx$key)
  start <- 'mice::make.predictorMatrix(df) |>\n  tibble::as_tibble(rownames = "target") |>\n'
  finish <- ' |>\n  tibble::column_to_rownames("target") |> as.matrix()'
  solution <- paste0(start, sprintf("  mutate(%s = 0)", ctx$key), finish)
  if (level > 1L) {
    inputs <- c("baseline", "auxiliary")
    if (level == 2L) {
      instruction <- sprintf(paste("Construct a named mice predictor matrix for a prospective workflow.",
        "The target row %s uses baseline and auxiliary only. The post-visit outcome is unavailable when predicting.",
        "Set every other entry to zero. Return df column order on both axes."), ctx$x)
    } else {
      inputs <- c(inputs, "baseline_aux", ctx$y)
      instruction <- sprintf(paste("In this retrospective study only exposure %s has gaps. The analysis regresses the recorded outcome %s",
        "on that exposure, baseline, auxiliary and their supplied baseline-by-auxiliary product baseline_aux.",
        "The specified linear conditional imputation model for the exposure uses the analysis outcome and all three covariate terms.",
        "Return the named mice predictor matrix in df column order on both axes. Enable exactly those four predictors for %s",
        "and set all other entries to zero, including the ID and post_visit fields."), ctx$x, ctx$y, ctx$x)
    }
    columns <- paste(inputs, collapse = ", ")
    solution <- paste0(start, sprintf(paste0('  mutate(across(-target, ~ 0),\n',
      '         across(c(%s), ~ as.numeric(target == "%s")))'), columns, ctx$x), finish)
  }
  exercise_plan(instruction, solution,
    if (level < 3L) "Each row selects predictors for one target. Think about which information is available when this workflow will run."
    else "Preserve the exposure's relationships with the recorded outcome and the analysis covariates, including the fully observed product term.",
    "Start from mice::make.predictorMatrix() to retain both dimension names.",
    'mice::make.predictorMatrix(df) |> tibble::as_tibble(rownames = "target") |> mutate(...)',
    if (level < 3L) "A column exclusion blocks that field everywhere. Row-specific entries let different targets use different information."
    else paste("For this linear study the recorded outcome informs the missing exposure, and both conditional models allow the baseline-by-auxiliary term.",
      "When the analysis instead involves an interaction or nonlinear transformation of the incomplete exposure itself, the joint model may require a specially constructed imputation method."),
    fixture = function(ctx, variant) {
      size <- sample(22:40, 1L)
      df <- data.frame(id = sample.int(10000, size), baseline = stats::rnorm(size),
        auxiliary = stats::rnorm(size), first = stats::rnorm(size, 30, 4), second = stats::rnorm(size, 50, 6),
        post_visit = stats::rnorm(size))
      names(df)[c(1L, 4L, 5L)] <- c(ctx$key, ctx$x, ctx$y)
      if (level == 3L) {
        df$baseline_aux <- df$baseline * df$auxiliary
        df[[ctx$x]] <- 30 + df$baseline - 0.4 * df$auxiliary + 0.5 * df$baseline_aux + stats::rnorm(size)
        df[[ctx$y]] <- 35 + 0.65 * df[[ctx$x]] + df$baseline + df$auxiliary - 0.5 * df$baseline_aux + stats::rnorm(size)
      }
      df[[ctx$x]][sample.int(size, 4L)] <- NA_real_
      list(df = df[sample(names(df))])
    })
}

mice_where_exercise <- function(ctx, level) {
  task <- "Start with df and return the named logical mice where matrix marking every missing cell for imputation."
  code <- "mice::make.where(df)"
  if (level > 1L) {
    over <- if (level == 3L) " | df$check_observed" else ""
    code <- sprintf(paste0('mice::make.where(df) |>\n  tibble::as_tibble(rownames = "row") |>\n',
      '  mutate(%s = df$eligible & (is.na(df$%s)%s)) |>\n  tibble::column_to_rownames("row") |> as.matrix()'),
                    ctx$x, ctx$x, over)
    task <- sprintf(paste("Return a logical mice where matrix with df's shape and names.",
      "Mark all missing cells, except %s is undefined when eligible is FALSE; never impute those structural cells.%s"),
      ctx$x, if (level == 3L) sprintf(" Additionally mark the observed %s cells for eligible records with check_observed TRUE for an overimputation diagnostic.", ctx$x) else "")
  }
  exercise_plan(task, code,
    "The mask addresses individual records and variables. A scheduled measurement can be missing, while an inapplicable measurement has no value to recover.",
    "mice::make.where(df) starts with the ordinary missing-cell pattern.",
    if (level == 1L) "mice::make.where(...)" else 'mice::make.where(df) |> tibble::as_tibble(rownames = "row") |> mutate(...)',
    "TRUE requests a draw for that cell. A structural skip remains FALSE. Deliberate overimputation compares a diagnostic draw against a recorded value.",
    fixture = function(ctx, variant) {
      size <- sample(28:39, 1L)
      eligible <- sample(rep(c(TRUE, TRUE, FALSE), length.out = size))
      x <- round(stats::rnorm(size, mean = 25, sd = 4), 2L)
      x[!eligible | seq_len(size) %in% sample(which(eligible), 5L)] <- NA_real_
      df <- data.frame(eligible, x, baseline = stats::rnorm(size))
      names(df)[2L] <- ctx$x
      df$baseline[sample.int(size, 3L)] <- NA_real_
      if (level == 3L) df$check_observed <- seq_len(size) %in% sample(which(!is.na(x)), 3L)
      list(df = df[sample(names(df))])
    })
}

# Generate a study with two incomplete-data patterns in each survey stratum.
# The outcome is fully measured and is available for retrospective estimation.
mice_study_frame <- function(ctx) {
  size <- sample(54:78, 1L)
  stratum <- sample(rep(c("East", "West"), length.out = size))
  baseline <- stats::rnorm(size, mean = 3, sd = 1.5)
  auxiliary <- stats::rnorm(size)
  measurement <- 25 + baseline + 0.6 * auxiliary + stats::rnorm(size, sd = 2)
  outcome <- 2 + 0.8 * measurement + 0.4 * baseline + stats::rnorm(size, sd = 2.4)
  missing <- unlist(lapply(split(seq_len(size), stratum), sample, size = 9L), use.names = FALSE)
  measurement[missing] <- NA_real_
  df <- data.frame(unit = sample.int(9999, size), stratum, baseline, auxiliary,
                   measurement, outcome, stringsAsFactors = TRUE)
  names(df)[5:6] <- c(ctx$x, ctx$y)
  df
}

mice_fit_exercise <- function(ctx, level) {
  df_args <- if (level == 1L) "" else ", predictorMatrix = predictors"
  run <- sprintf("fit <- mice::mice(df, m = draws, maxit = cycles, seed = fit_seed%s%s, printFlag = FALSE)",
                 df_args, if (level == 3L) ", where = where" else "")
  result <- if (level == 1L) "mice::complete(fit, action = 1)" else 'mice::complete(fit, action = "long")'
  task <- paste(
    sprintf("Run mice::mice() on df, with draws completed data sets, cycles iterations and the supplied fit_seed. Use predictive mean matching for %s, with default donor and visit settings.", ctx$x),
    if (level == 1L) "Use the default predictor matrix. Return the first completed data frame in original row order."
    else sprintf("Use the supplied predictors matrix, which permits only baseline and auxiliary as predictors of %s. The post_visit field is unavailable at prediction time.", ctx$x),
    if (level == 3L) sprintf("Use the supplied where mask to leave %s missing whenever eligible is FALSE because the measurement does not apply.", ctx$x) else "",
    if (level > 1L) 'Return every completed data set in mice long format, including .imp and .id, without the original incomplete rows.' else "")
  exercise_plan(trimws(task), paste(run, result, sep = "\n"),
    "Configure the data available for imputation before running the chains. Extract completed records after the run.",
    "Use mice::mice() with the supplied m, maxit and seed, followed by mice::complete().",
    "fit <- mice::mice(df, m = draws, maxit = cycles, seed = fit_seed, ...)\nmice::complete(fit, ...)",
    paste("The explicit seed makes this PMM run reproducible. The small teaching runs keep practice fast. Increase m for more stable pooled estimates across repeated draws.",
      "Increase maxit when chains need more updates before settling. A collinearity removal in loggedEvents calls for reviewing the affected predictor model."),
    exercise_table(ordered = level == 1L),
    fixture = function(ctx, variant) {
      study <- mice_study_frame(ctx)
      df <- study[c("baseline", "auxiliary", ctx$x)]
      if (level > 1L) df$post_visit <- study[[ctx$y]]
      if (level == 3L) {
        df$eligible <- sample(rep(c(TRUE, TRUE, TRUE, FALSE), length.out = nrow(df)))
        df[[ctx$x]][!df$eligible] <- NA_real_
      }
      inputs <- list(df = df, draws = sample(2:3, 1), cycles = sample(1:2, 1),
                     fit_seed = sample.int(1000000L, 1))
      if (level > 1L) {
        inputs$predictors <- mice::make.predictorMatrix(df)
        inputs$predictors[,] <- 0
        inputs$predictors[ctx$x, c("baseline", "auxiliary")] <- 1
      }
      if (level == 3L) {
        inputs$where <- mice::make.where(df)
        inputs$where[, ctx$x] <- inputs$where[, ctx$x] & df$eligible
      }
      inputs
    })
}

mice_fitted_fixture <- function(ctx, variant) {
  df <- mice_study_frame(ctx)
  pred <- mice::make.predictorMatrix(df)
  pred[,] <- 0
  pred[ctx$x, c("baseline", "auxiliary", ctx$y, "stratum")] <- 1
  fit <- mice::mice(df, m = 3L, maxit = 4L, predictorMatrix = pred,
                    seed = sample.int(1000000L, 1L), printFlag = FALSE)
  # All formula symbols are data columns. Discard references to the generator's
  # environment so the saved fixture is small and identical for a repeated seed.
  fit$formulas <- lapply(fit$formulas, function(formula) {
    environment(formula) <- baseenv()
    formula
  })
  fit$date <- as.Date(NA_character_) # The fixture describes a synthetic study without a creation date.
  list(imp = fit)
}

mice_complete_exercise <- function(ctx, level) {
  number <- sample(2:3, 1L)
  task <- sprintf("imp is a fitted mice mids object. Extract completed data set %d and return all its columns and rows in their original order.", number)
  code <- sprintf("mice::complete(imp, action = %d)", number)
  if (level == 2L) {
    task <- sprintf("Stack the original and every completed data set from imp. Return only .imp, unit and %s in that column order. Keep the original records under .imp zero.", ctx$x)
    code <- sprintf('mice::complete(imp, action = "long", include = TRUE) |>\n  select(.imp, unit, %s)', ctx$x)
  } else if (level == 3L) {
    task <- sprintf("For each unit whose %s was originally missing in imp, report its minimum and maximum imputed %s across completed data sets. Return ungrouped columns unit, low and high.", ctx$x, ctx$x)
    code <- sprintf(paste0('missing_units <- imp$data |> filter(is.na(%s)) |> pull(unit)\n',
      'mice::complete(imp, action = "long") |>\n  filter(unit %%in%% missing_units) |>\n',
      '  summarise(low = min(%s), high = max(%s), .by = unit)'), ctx$x, ctx$x, ctx$x)
  }
  exercise_plan(task, code,
    "The original incomplete records identify which cells were filled. Each completed data set retains the same people.",
    "Use mice::complete() with a draw number or action = \"long\". include = TRUE makes the original data available as draw zero.",
    if (level == 1L) "mice::complete(imp, action = ...)" else 'mice::complete(imp, action = "long", ...) |> ...',
    "Draw numbers identify alternative completions for the same records. A within-unit range describes the spread of that cell's imputed values.",
    exercise_table(ordered = level == 1L, column_order = level == 2L), mice_fitted_fixture)
}

mice_chain_exercise <- function(ctx, level) {
  task <- sprintf("Inspect the final stored iteration for %s in imp. Return one row per chain with character chain (its original chain name) and chain_mean (the mean of the imputed values).", ctx$x)
  code <- sprintf('imp$chainMean["%s", imp$iteration, ] |>\n  tibble::enframe(name = "chain", value = "chain_mean")', ctx$x)
  if (level == 2L) {
    task <- sprintf("Inspect the spread of imputed %s values within each chain in imp. At every iteration, compute the smallest and largest of the chain standard deviations. Return numeric iteration numbers in iteration and the two standard deviations in low and high.", ctx$x)
    code <- sprintf(paste0('sqrt(imp$chainVar["%s", , ]) |>\n  tibble::as_tibble(rownames = "iteration") |>\n',
      '  mutate(iteration = as.integer(iteration)) |>\n  pivot_longer(-iteration, values_to = "chain_sd") |>\n',
      '  summarise(low = min(chain_sd), high = max(chain_sd), .by = iteration)'), ctx$x)
  } else if (level == 3L) {
    task <- sprintf("For %s in imp, reconstruct the chain-mean trace for every iteration of every chain. Return numeric iteration numbers in iteration, character chain using the supplied names, chain_mean, and change (this mean minus the preceding mean within that chain). The first change is missing.", ctx$x)
    code <- sprintf(paste0('tibble::as_tibble(imp$chainMean["%s", , ], rownames = "iteration") |>\n',
      '  mutate(iteration = as.integer(iteration)) |>\n',
      '  pivot_longer(-iteration, names_to = "chain", values_to = "chain_mean") |>\n',
      '  arrange(chain, iteration) |>\n  mutate(change = chain_mean - lag(chain_mean), .by = chain)'), ctx$x)
  }
  exercise_plan(task, code,
    "Examine how the summaries move over iterations and whether chains mix. Keep the iteration order within each chain.",
    "The mids arrays chainMean and chainVar have axes for variable, iteration and chain. A square root converts stored within-chain variance to standard deviation.",
    if (level == 1L) sprintf('imp$chainMean["%s", imp$iteration, ] |> ...', ctx$x)
    else sprintf('trace <- imp$%s["%s", , ]', if (level == 2L) "chainVar" else "chainMean", ctx$x),
    "A persistent trend or separation merits a longer run and investigation. Four teaching iterations provide practice reading diagnostics and cannot establish convergence.",
    exercise_table(), mice_fitted_fixture)
}

mice_comparison_exercise <- function(ctx, level) {
  draw <- sample(1:3, 1L)
  keys <- switch(level, "source", "c(.imp, source)", "c(.imp, stratum, source)")
  outputs <- if (level == 1L) "records = n(), average = mean(%s)" else "records = n(), average = mean(%s), spread = sd(%s)"
  summary <- if (level == 1L) sprintf(outputs, ctx$x) else sprintf(outputs, ctx$x, ctx$x)
  code <- sprintf(paste0('observed <- imp$data |> filter(!is.na(%s)) |> pull(unit)\n',
    '%s |>\n  mutate(source = if_else(unit %%in%% observed, "observed", "imputed")) |>\n',
    '  summarise(%s, .by = %s)'), ctx$x,
    if (level == 1L) sprintf("mice::complete(imp, %d)", draw) else 'mice::complete(imp, "long")', summary, keys)
  task <- paste(sprintf('Compare the distribution of %s in imp by original cell status. Label source as "observed" or "imputed" from the original data.', ctx$x),
    switch(level, sprintf("Use only completion %d. Return one row per source with records (count) and average (mean).", draw),
           "Return one row per .imp and source with records (count), average (mean), and spread (sample standard deviation). Exclude draw zero.",
           "Compare within each survey stratum. Return one row per .imp, stratum and source with records (count), average (mean), and spread (sample standard deviation). Exclude draw zero."))
  exercise_plan(task, code,
    "Label the origin of cells before summarizing them. Originally observed values stay fixed across completions.",
    "Use imp$data to identify observed units, then combine mice::complete() with mutate() and summarise().",
    sprintf('observed <- imp$data |> filter(!is.na(%s)) |> pull(unit)', ctx$x),
    "Differences can reflect the observed predictors of missingness. Look for implausible values, dispersion or subgroup patterns. Distributional agreement alone cannot establish MAR.",
    exercise_table(), mice_fitted_fixture)
}

mice_pool_exercise <- function(ctx, level) {
  formula <- sprintf("%s ~ %s%s", ctx$y, ctx$x, if (level > 1L) " + baseline" else "")
  model <- sprintf("fits <- with(imp, lm(%s))\npooled <- mice::pool(fits)", formula)
  if (level == 1L) {
    output <- 'summary(pooled) |>\n  transmute(term = as.character(term), estimate, std.error)'
    request <- "Return every coefficient including the intercept with character term, pooled estimate and pooled std.error."
  } else if (level == 2L) {
    output <- 'summary(pooled, conf.int = TRUE, conf.level = 0.95) |>\n  transmute(term = as.character(term), estimate, std.error, df, conf.low, conf.high)'
    request <- paste("For every coefficient return character term, pooled estimate, std.error,",
      "Rubin pooling degrees of freedom df with the default small-sample adjustment, and the two-sided 95% confidence limits conf.low and conf.high.")
  } else {
    output <- sprintf(paste0('pooled$pooled |>\n  filter(term == "%s") |>\n',
      '  transmute(term = as.character(term), estimate, within = ubar, between = b,\n',
      '            total = t, std.error = sqrt(t), lambda, fmi)'), ctx$x)
    request <- sprintf(paste("For the %s coefficient alone return character term, estimate, within (mean within-imputation variance),",
      "between (between-imputation variance), total (Rubin total variance), std.error, lambda (share of total variance due to missingness)",
      "and fmi (finite-sample fraction of missing information)."), ctx$x)
  }
  exercise_plan(sprintf("Fit the linear regression %s separately in every completed data set in imp, then pool under Rubin's rules. %s", formula, request),
    paste(model, output, sep = "\n"),
    "Estimate the same model in each completion so both sampling uncertainty and differences between estimates contribute to inference.",
    "with(imp, lm(...)) fits each completed data set. mice::pool() combines its estimates and variances.",
    sprintf("fits <- with(imp, lm(%s))\npooled <- mice::pool(...)", formula),
    "Rubin pooling averages the coefficient estimates and combines within- and between-imputation variance. The pooled standard error is the square root of total variance.",
    exercise_table(), mice_fitted_fixture)
}

exercise_library_missing_mice <- function() {
  category <- "Missing Data"
  list(
    mice_methods = exercise_template(category, "Choose imputation methods by type", 1L, mice_method_exercise),
    mice_predictors = exercise_template(category, "Design an imputation predictor matrix", 1L, mice_predictor_exercise),
    mice_where = exercise_template(category, "Mark eligible imputation cells", 1L, mice_where_exercise),
    mice_fit = exercise_template(category, "Run chained equations", 2L, mice_fit_exercise),
    mice_complete = exercise_template(category, "Extract completed data sets", 2L, mice_complete_exercise),
    mice_chains = exercise_template(category, "Inspect imputation chains", 3L, mice_chain_exercise),
    mice_distributions = exercise_template(category, "Compare observed and imputed values", 2L, mice_comparison_exercise),
    mice_pool = exercise_template(category, "Pool fitted models and uncertainty", 3L, mice_pool_exercise)
  )
}
