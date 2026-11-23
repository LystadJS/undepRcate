# These drills specify the decision or identifying assumption explicitly. The
# generated data alone cannot establish a source's authority or a response model.
resolution_code <- function(...) paste(c(...), collapse = "\n")

resolution_sources <- function(ctx, level) {
  instructions <- paste(
    "Two registries supply the same count. The signed register takes precedence over the field log.",
    "Return id and resolved from df. Keep a missing count if both sources are blank.",
    if (level >= 2L) "Add source with values 'signed', 'field' or 'unknown' identifying the source used.",
    if (level == 3L) "A disputed field log is ineligible as a fallback. Also add review, TRUE for unequal nonmissing reports or any nonmissing disputed field report."
  )
  solution <- resolution_code(
    "df |>", if (level == 3L)
      "  mutate(fallback = if_else(disputed, NA_integer_, field)) |>",
    paste0("  transmute(id, resolved = coalesce(signed, ", if (level == 3L) "fallback" else "field", ")",
           if (level == 1L) ")" else ","),
    if (level >= 2L) paste0("    source = case_when(!is.na(signed) ~ 'signed', !is.na(resolved) ~ 'field', TRUE ~ 'unknown')",
                            if (level == 2L) ")" else ","),
    if (level == 3L) "    review = !is.na(field) & (disputed | (!is.na(signed) & signed != field)))"
  )
  exercise_plan(instructions, solution,
    "Choose an eligible value in source-priority order. Two missing reports leave the count unknown.",
    "Use coalesce() for the priority rule and case_when() to label provenance when requested.",
    "df |> transmute(id, resolved = coalesce(...), ...)",
    "The signed count survives even when sources disagree. Provenance and review flags show where human verification may change a record.",
    exercise_table(), function(ctx, variant) {
      counts <- sample(4:70, 8)
      df <- tibble::tibble(id = sample(101:999, 8), signed = counts, field = counts,
                          disputed = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE))
      df$signed[c(2, 3, 5, 8)] <- NA_integer_
      df$field[c(5, 6)] <- NA_integer_
      df$field[c(4, 7)] <- df$field[c(4, 7)] + c(1L, 3L)
      if (variant >= 2L) df$field[1] <- df$field[1] + 2L
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_identity <- function(ctx, level) {
  solution <- resolution_code(
    "df |>", "  mutate(remainder = shipped - returned,",
    "         recoverable = applicable & !is.na(remainder) & remainder >= 0,",
    "         resolved = case_when(!applicable ~ NA_integer_, !is.na(kept) ~ kept,",
    "                              recoverable ~ remainder, TRUE ~ NA_integer_)) |>",
    if (level >= 2L) c("  mutate(status = case_when(",
      "    !applicable ~ 'structural',",
      "    !is.na(remainder) & (remainder < 0 | (!is.na(kept) & kept != remainder)) ~ 'conflict',",
      "    !is.na(kept) ~ 'recorded', recoverable ~ 'reconstructed', TRUE ~ 'unknown')) |>"),
    if (level == 3L) c("  mutate(kept_share = if_else(status %in% c('recorded', 'reconstructed') &",
      "                             !is.na(shipped) & shipped > 0 & resolved <= shipped,",
      "                             resolved / shipped, NA_real_)) |>"),
    paste0("  select(id, resolved", if (level >= 2L) ", status", if (level == 3L) ", kept_share", ")"))
  exercise_plan(paste("A fully accounted shipment has shipped units equal to returned plus kept units. Counts are nonnegative.",
    "In df return id and resolved. For inapplicable rows use missing. Otherwise preserve recorded kept.",
    "Reconstruct missing kept only from known shipped and returned when their difference is nonnegative.",
    if (level >= 2L) paste("Assign status using the first applicable label from this ordered list. Use 'structural' if inapplicable,",
      "'conflict' for a known negative difference or disagreement with recorded kept, 'recorded' for a supplied kept,",
      "'reconstructed' for a derived count, otherwise 'unknown'."),
    if (level == 3L) paste("Add kept_share as resolved divided by shipped only for recorded/reconstructed rows",
      "with known positive shipped and resolved no larger than shipped. Use missing for other rows.")),
    solution, "Check whether the accounting identity applies before using subtraction.",
    "Use mutate() to compute a candidate remainder, then case_when() to choose eligible values.",
    "df |> mutate(remainder = shipped - returned, resolved = case_when(...)) |> ...",
    "The accounting identity supplies one missing component only when enough valid inputs are known. A zero denominator leaves a derived share undefined.",
    exercise_table(), function(ctx, variant) {
      shipped <- sample(4:35, 9)
      returned <- sample(0:3, 9, TRUE)
      kept <- shipped - returned
      kept[c(1, 3, 5, 6, 7)] <- NA_integer_
      kept[2] <- kept[2] + 1L
      shipped[5] <- NA_integer_
      returned[6] <- shipped[6] + variant
      shipped[7] <- returned[7] <- 0L
      returned[9] <- NA_integer_
      df <- tibble::tibble(id = sample(100:999, 9), applicable = seq_along(kept) != 3,
                           shipped, returned, kept)
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_eligibility <- function(ctx, level) {
  solution <- resolution_code("df |> filter(eligible) |>",
    "  mutate(complete = if_all(c(score, exposure, baseline), ~ !is.na(.x))) |>",
    if (level >= 2L) "  group_by(site) |>",
    "  summarise(eligible_n = n(), complete_n = sum(complete),",
    paste0("            excluded_n = sum(!complete)", if (level >= 2L) ", retained_share = mean(complete)" else "", ","),
    if (level == 3L) c("            complete_mean = if (any(complete)) mean(score[complete]) else NA_real_,",
      "            available_mean = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE),"),
    "            .groups = 'drop')")
  exercise_plan(paste(
    "Only eligible records enter the study. The planned analysis needs score, exposure and baseline.",
    "Ignore missing values in the optional notes field. Return eligible_n, complete_n and excluded_n for eligible rows",
    if (level == 1L) "in a single row." else "within each represented site, with site and retained_share (complete divided by eligible).",
    if (level == 3L) "Add complete_mean (mean score among complete records) and available_mean (mean among all eligible records with observed score). Use NA_real_ for a mean with no records."),
    solution, "Define the denominator first. Completeness concerns only the variables the analysis needs.",
    "Combine filter(), if_all() and summarise(). Sum a logical flag to count qualifying rows.",
    "df |> filter(eligible) |> mutate(complete = if_all(c(...), ~ !is.na(.x))) |> ...",
    "This comparison quantifies attrition and changes in the observed sample mean. It cannot establish that either mean is unbiased for all eligible records.",
    exercise_table(), function(ctx, variant) {
      size <- 24L
      df <- tibble::tibble(site = rep(c("East", "West", "North"), each = 8L),
        eligible = rep(c(TRUE, TRUE, FALSE, TRUE), 6L), score = sample(15:95, size, TRUE),
        exposure = sample(0:1, size, TRUE), baseline = sample(20:50, size, TRUE),
        notes = sample(c("checked", NA_character_), size, TRUE))
      df$score[c(1, 10)] <- df$exposure[c(2, 17)] <- df$baseline[c(8, 12)] <- NA_integer_
      if (variant == 2L) df$baseline[df$site == "North"] <- NA_integer_
      if (variant == 3L) df$score[df$site == "West"] <- NA_integer_
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_carry <- function(ctx, level) {
  limit <- ctx$by
  solution <- resolution_code("df |> arrange(device, shift, hour) |> group_by(device, shift) |>",
    if (level == 3L) "  mutate(run = cumsum(reset)) |> group_by(device, shift, run) |>",
    "  mutate(resolved = setting, last_read = if_else(!is.na(setting), hour, NA_integer_)) |>",
    "  fill(resolved, last_read, .direction = 'down') |>",
    if (level >= 2L) sprintf("  mutate(resolved = if_else(hour - last_read <= %d, resolved, NA_integer_)) |>", limit),
    "  ungroup() |> select(id, resolved)")
  exercise_plan(paste("In df, device settings persist until changed within a single device and shift.",
    "Return id and resolved using only readings at or before the row's hour. Leave gaps before the first reading missing.",
    if (level >= 2L) sprintf("A reading expires more than %d hours after its actual measurement. Measure the gap from the last recorded reading.", limit),
    if (level == 3L) "A reset clears the remembered value before processing that row. A new reading on the reset row is valid."),
    solution, "Sort records by time. Keep each device and shift on its own timeline.",
    "Use arrange(), group_by() and fill() downward. Carry the last actual reading time for an expiry rule.",
    "df |> arrange(device, shift, hour) |> group_by(device, shift) |> ...",
    "The equipment rule authorizes carry-forward within the specified window. A leading gap stays missing because no earlier reading is available.",
    exercise_table(), function(ctx, variant) {
      df <- tidyr::expand_grid(device = c("North", "South"), shift = c("AM", "PM"),
        hour = c(0L, 1L, 2L, 3L, limit + 4L, limit + 5L, limit + 6L))
      df$id <- sample(1000:9999, nrow(df))
      df$setting <- sample(1:20, nrow(df), TRUE)
      df$setting[!df$hour %in% c(1L, limit + 5L)] <- NA_integer_
      df$reset <- df$hour %in% c(3L, limit + 5L)
      if (variant == 2L) df$setting[df$shift == "PM"] <- NA_integer_
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_weight <- function(ctx, level) {
  solution <- resolution_code(
    "df |> filter(!is.na(score)) |> mutate(weight = 1 / p_observed) |>",
    if (level >= 2L) "  group_by(site) |>",
    "  summarise(weighted_mean = sum(weight * score) / sum(weight),",
    if (level >= 2L) "            respondent_mean = mean(score),",
    if (level == 3L) c("            effective_n = sum(weight)^2 / sum(weight^2),",
                       "            respondents = n(),"),
    "            .groups = 'drop')")
  exercise_plan(paste(
    "Only score can be missing in df. Every participant has a recorded site, stratum and known positive observation probability p_observed.",
    "Assume response is independent of the otherwise missing score given those recorded characteristics, and the probabilities are correct.",
    "Estimate mean score with inverse observation-probability weights. Divide the weighted sum by the sum of respondent weights.",
    if (level == 1L) "Return one row named weighted_mean." else
      "Return one row per site with site, weighted_mean and respondent_mean (unweighted mean among respondents).",
    if (level == 3L) "Add respondents and effective_n. For effective_n square the sum of weights and divide by the sum of squared weights."),
    solution, "Rows with observed scores represent themselves and other units with the same response propensity.",
    "Filter to observed scores, invert the observation probabilities with mutate(), and compute a ratio using summarise().",
    "df |> filter(!is.na(score)) |> mutate(weight = 1 / p_observed) |> ...",
    "For this single incomplete outcome, the stated conditional independence is the missing-at-random assumption within site and stratum. Normalization produces a weighted respondent mean. The effective sample size measures weight concentration. It cannot verify that scores are missing at random.",
    exercise_table(), function(ctx, variant) {
      df <- tidyr::expand_grid(site = c("East", "West"), stratum = c("A", "B"), unit = 1:10)
      df$score <- sample(10:30, nrow(df), TRUE) + 30L * (df$stratum == "B")
      df <- df |>
        dplyr::group_by(site, stratum) |>
        dplyr::mutate(quota = dplyr::if_else(stratum == "A", 2L + variant, 9L),
                      p_observed = quota / dplyr::n(),
                      score = dplyr::if_else(sample.int(dplyr::n()) <= quota, score, NA_real_)) |>
        dplyr::ungroup() |> dplyr::select(-quota)
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_delta <- function(ctx, level) {
  solution <- resolution_code(
    if (level == 1L) "df |> summarise(estimate = mean(coalesce(score, prediction + shift)))" else
      c("df |> cross_join(scenarios) |> group_by(shift) |>",
        "  summarise(estimate = mean(coalesce(score, prediction + shift)), .groups = 'drop') |>",
        paste0("  mutate(below = estimate < threshold)", if (level == 3L) " |>")),
    if (level == 3L) c("  filter(below) |>",
      "  summarise(tipping_shift = if (n() == 0L) NA_real_ else max(shift))"))
  exercise_plan(paste("Assess sensitivity of the cohort mean to departures from a baseline missing-outcome prediction.",
    "Keep observed scores. For each missing score, assume its expected value equals prediction plus the specified shift in score units.",
    if (level == 1L) "Use the single supplied shift. Return a one-row table with estimate, the mean over all rows."
    else paste("Use every shift in scenarios (zero and negative shifts). Compare the mean over all rows with threshold.",
      if (level == 2L) "Return shift, estimate and below, the logical flag for a mean strictly below threshold."
      else "Return one row with tipping_shift, the closest tested shift to zero with a mean strictly below threshold. Use NA_real_ if no tested shift qualifies.")),
    solution, "Shift only the missing portion. Scores that were actually measured stay fixed in every scenario.",
    "Use coalesce() to preserve measurements. For several scenarios, cross_join() and group by shift before summarising.",
    if (level == 1L) "df |> summarise(estimate = mean(coalesce(score, ...)))" else
      "df |> cross_join(scenarios) |> group_by(shift) |> summarise(estimate = ...) |> ...",
    "Each result is a scenario mean under a stated displacement. The data do not identify the shift. A grid tipping point applies only to the tested shifts.",
    exercise_table(), function(ctx, variant) {
      count <- sample(14:25, 1)
      df <- tibble::tibble(score = sample(50:90, count, TRUE), prediction = sample(50:90, count, TRUE))
      df$score[sample.int(count, floor(count / 3))] <- NA_integer_
      steps <- seq(0, -5 * ctx$by, by = -ctx$by)
      if (level == 1L) return(list(df = df, shift = sample(steps[-1], 1)))
      center <- mean(dplyr::coalesce(df$score, df$prediction))
      threshold <- switch(variant, center - ctx$by * mean(is.na(df$score)) * 1.5,
                          center - max(abs(steps)) - 1, center + 1)
      list(df = df, scenarios = tibble::tibble(shift = sample(steps)), threshold = threshold)
    })
}

resolution_bounds <- function(ctx, level) {
  solution <- resolution_code("df |>",
    if (level >= 2L) "  group_by(cohort) |>",
    "  summarise(lower = sum(event == 1, na.rm = TRUE) / n(),",
    paste0("            upper = (sum(event == 1, na.rm = TRUE) + sum(is.na(event))) / n(), .groups = 'drop')",
           if (level == 3L) " |>"),
    if (level == 3L) c("  summarise(lower_diff = lower[cohort == 'B'] - upper[cohort == 'A'],",
      "            upper_diff = upper[cohort == 'B'] - lower[cohort == 'A'])"))
  exercise_plan(paste(
    "event in df is binary and each missing event can independently be zero or one.",
    "Use all enrolled units as the denominator. Compute the sharp range of the finite cohort's realized event fraction",
    if (level == 1L) "as one row with lower and upper."
    else if (level == 2L) "within each cohort, returning cohort, lower and upper."
    else "in each cohort, then bound the difference in fractions (B minus A). Return one row with lower_diff and upper_diff."),
    solution, "Assign every unknown outcome to each extreme. Keep every unit in the denominator.",
    "Use sum() to count observed events and missing outcomes, then divide by n() within the target cohort.",
    "df |> summarise(lower = sum(event == 1, na.rm = TRUE) / n(), ...)",
    "Both endpoints are attainable assignments of the missing binary values. The range describes missing-outcome ambiguity for these enrolled units and has no confidence level.",
    exercise_table(), function(ctx, variant) {
      df <- tibble::tibble(cohort = rep(c("A", "B"), c(9L, 12L)), event = sample(0:1, 21L, TRUE))
      df$event[c(1, 2, 6, 12)] <- NA_integer_
      if (variant == 2L) df$event[df$cohort == "B"] <- NA_integer_
      if (variant == 3L) df$event[df$cohort == "A"] <- sample(0:1, 9L, TRUE)
      list(df = dplyr::slice_sample(df, prop = 1))
    })
}

resolution_report_estimates <- function(respondents, rows) {
  score <- respondents$score[rows]
  weight <- 1 / respondents$p_observed[rows]
  c(respondents = mean(score), weighted = sum(score * weight) / sum(weight))
}

resolution_report <- function(ctx, level) {
  solution <- switch(level,
    "point |> pivot_wider(names_from = method, values_from = estimate) |>\n  transmute(respondents, weighted, change = weighted - respondents)",
    resolution_code("replicates |> group_by(method) |>",
      "  summarise(lower = quantile(estimate, .025, names = FALSE),",
      "            upper = quantile(estimate, .975, names = FALSE), .groups = 'drop') |>",
      "  left_join(point, by = 'method') |> select(method, estimate, lower, upper)"),
    resolution_code("paired <- replicates |> pivot_wider(names_from = method, values_from = estimate) |>",
      "  mutate(change = weighted - respondents)",
      "point |> pivot_wider(names_from = method, values_from = estimate) |>",
      "  transmute(change = weighted - respondents,",
      "            lower = quantile(paired$change, .025, names = FALSE),",
      "            upper = quantile(paired$change, .975, names = FALSE),",
      "            includes_zero = lower <= 0 & upper >= 0)"))
  exercise_plan(paste(
    "The analysis team compared the unweighted respondent mean and an inverse observation-probability weighted mean.",
    "point supplies their original-sample estimates, with method labels respondents and weighted.",
    if (level >= 2L) "The bootstrap samples respondents with replacement within response strata, holding respondent counts and observation probabilities fixed.",
    switch(level,
      "Return one row with respondents, weighted and change (weighted minus respondents).",
      paste("replicates holds both estimates from each shared bootstrap sample. For each method return method, the original estimate,",
        "and lower and upper as the 2.5th and 97.5th percentiles of its bootstrap estimates. Use R's default quantiles."),
      paste("replicates holds both estimates from each shared bootstrap sample, identified by draw.",
        "Return one row with original-sample change (weighted minus respondents), lower and upper as the 2.5th and 97.5th",
        "percentiles of the paired bootstrap changes (R's default quantiles), and includes_zero indicating whether either endpoint equals zero or the endpoints straddle zero."))),
    solution, "Keep the original estimate separate from the distribution across repeated samples.",
    "Use pivot_wider() to align methods by their shared draw before subtracting. Use group_by() for separate method intervals.",
    if (level == 2L) "replicates |> group_by(method) |> summarise(lower = quantile(estimate, ...), ...)" else
      "point |> pivot_wider(names_from = method, values_from = estimate) |> ...",
    "A paired difference preserves the association between methods on the same resample. Bootstrap variation describes sampling uncertainty under the supplied resampling design. Assess uncertainty about the response assumptions separately using sensitivity scenarios.",
    exercise_table(), function(ctx, variant) {
      cohort <- tidyr::expand_grid(stratum = c("A", "B"), unit = seq_len(20L)) |>
        dplyr::group_by(stratum) |>
        dplyr::mutate(quota = dplyr::if_else(stratum == "A", 14L + variant, 5L + variant),
          p_observed = quota / dplyr::n(), observed = sample.int(dplyr::n()) <= quota,
          score = sample(30:70, dplyr::n(), TRUE) + 12L * (stratum == "B")) |>
        dplyr::ungroup()
      respondents <- dplyr::filter(cohort, observed)
      ids <- seq_len(nrow(respondents))
      original <- resolution_report_estimates(respondents, ids)
      point <- tibble::tibble(method = names(original), estimate = unname(original))
      if (level == 1L) return(list(point = dplyr::slice_sample(point, prop = 1)))
      strata <- split(ids, respondents$stratum)
      size <- sample(120:200, 1)
      paired <- replicate(size, {
        sampled_ids <- unlist(lapply(strata, function(group) sample(group, length(group), replace = TRUE)),
                              use.names = FALSE)
        resolution_report_estimates(respondents, sampled_ids)
      })
      samples <- tibble::tibble(draw = seq_len(size), respondents = paired["respondents", ],
                                weighted = paired["weighted", ]) |>
        tidyr::pivot_longer(-draw, names_to = "method", values_to = "estimate") |>
        dplyr::slice_sample(prop = 1)
      list(point = point, replicates = samples)
    })
}

resolution_train_only <- function(ctx, level) {
  solution <- switch(level,
    resolution_code("center <- train |> summarise(value = median(length_cm, na.rm = TRUE)) |> pull(value)",
      "heldout |> transmute(id, filled = coalesce(length_cm, center))"),
    resolution_code("center <- train |> summarise(value = median(length_cm, na.rm = TRUE)) |> pull(value)",
      "local <- train |> filter(!is.na(length_cm)) |> group_by(site) |>",
      "  summarise(site_center = median(length_cm), .groups = 'drop')",
      "heldout |> left_join(local, by = 'site') |>",
      "  transmute(id, filled = coalesce(length_cm, site_center, center))"),
    resolution_code("known <- train |> pivot_longer(c(length_cm, mass_kg), names_to = 'feature') |>",
      "  filter(!is.na(value))",
      "local <- known |> group_by(site, feature) |> summarise(site_center = median(value), .groups = 'drop')",
      "global <- known |> group_by(feature) |> summarise(center = median(value), .groups = 'drop')",
      "heldout |> pivot_longer(c(length_cm, mass_kg), names_to = 'feature') |>",
      "  left_join(local, by = c('site', 'feature')) |> left_join(global, by = 'feature') |>",
      "  transmute(id, feature, was_missing = is.na(value), filled = coalesce(value, site_center, center),",
      "            source = case_when(!was_missing ~ 'observed', !is.na(site_center) ~ 'site', TRUE ~ 'global'))"))
  exercise_plan(paste(
    "You are preparing predictors for a held-out evaluation sample. Learn replacement values from observed training entries in train.",
    "Apply the frozen rule to heldout. Preserve every observed held-out entry.",
    switch(level,
      "Replace missing length_cm using the overall training median. Return id and filled for every held-out row.",
      paste("Replace missing length_cm with the matching site's training median. If that site is new or supplied no training lengths,",
            "use the overall training median. Return id and filled for every held-out row."),
      paste("Process length_cm and mass_kg separately. For each missing value prefer its site-and-feature training median,",
            "then its feature-wide training median. Each feature has training measurements somewhere.",
            "Return one row per held-out id and feature, with feature equal to its original column name, filled,",
            "a logical was_missing flag from the original entry, and source ('observed', 'site' or 'global')."))),
    solution, "The held-out predictors represent future data. Their distribution must stay out of the fitted preprocessing rule.",
    "Calculate training medians with summarise(). Join the saved values to heldout and use coalesce() for the fallback.",
    if (level == 3L) "train |> pivot_longer(c(length_cm, mass_kg), names_to = 'feature') |> filter(!is.na(value)) |> ..."
    else "train |> summarise(value = median(length_cm, na.rm = TRUE))",
    "Training-only medians make the evaluation match deployment to new cases. An original missingness flag preserves information that a replacement would otherwise erase. Its predictive value needs evaluation.",
    exercise_table(), function(ctx, variant) {
      train <- tibble::tibble(
        site = rep(c("Inlet", "Ridge", "Plain"), each = 5L),
        length_cm = as.double(sample(10:24) + rep(c(20, 0, 50), each = 5)),
        mass_kg = as.double(sample(50:64) + rep(c(0, 40, 5), each = 5))
      )
      train$length_cm[c(2, 11:15)] <- NA_real_
      train$mass_kg[c(1:5, 8)] <- NA_real_
      heldout <- tibble::tibble(id = sample(100:999, 16),
        site = rep(c("Inlet", "Ridge", "Plain", "New"), times = 4),
        length_cm = sample(105:190, 16), mass_kg = sample(205:290, 16))
      heldout$length_cm[c(1:4, 12)] <- NA_integer_
      heldout$mass_kg[c(5:8, 10)] <- NA_integer_
      if (variant == 2L) heldout$site[heldout$site == "Ridge"] <- "Another"
      if (variant == 3L) heldout$length_cm[heldout$site == "New"] <- NA_integer_
      list(train = dplyr::slice_sample(train, prop = 1), heldout = dplyr::slice_sample(heldout, prop = 1))
    })
}

resolution_mask_validation <- function(ctx, level) {
  solution <- resolution_code("evaluation |> filter(masked, !is.na(truth)) |>",
    if (level == 1L) c("  filter(method == 'local', !is.na(prediction)) |>",
      "  summarise(scored_n = n(), mae = mean(abs(prediction - truth)))")
    else c(
      if (level == 2L) "  filter(method == 'local', !is.na(prediction), !is.na(lower), !is.na(upper)) |>"
      else c("  group_by(site) |> mutate(eligible_n = n_distinct(id)) |> ungroup() |>",
        "  group_by(id) |> filter(n_distinct(method) == 2L, all(!is.na(prediction) & !is.na(lower) & !is.na(upper))) |> ungroup() |>"),
      if (level == 2L) "  group_by(site) |>" else "  group_by(site, method, eligible_n) |>",
      "  summarise(scored_n = n(), mae = mean(abs(prediction - truth)),",
      "            signed_error = mean(prediction - truth),",
      "            coverage = mean(lower <= truth & truth <= upper), .groups = 'drop')"))
  exercise_plan(paste("The evaluation team deliberately masked some previously measured values before fitting either method.",
    "evaluation has at most one row per id and method. truth records the original measurement where available.",
    "masked is TRUE only on deliberately withheld measurements. Genuine gaps have unknown truth. Visible measurements must stay out of the score.",
    switch(level,
      "For the local method only, return one row with scored_n and mae (mean absolute prediction error) for masked rows with predictions.",
      paste("For the local method, score masked rows with a prediction and both prediction-interval endpoints.",
        "For each represented site return site, scored_n, mae, signed_error (mean prediction minus truth),",
        "and coverage (fraction of truths between lower and upper inclusive)."),
      paste("Compare local and pooled on the same ids. Include an id only when both methods supply predictions and both endpoints.",
        "For each site and method with scored pairs, return site, method, eligible_n (distinct masked ids at the site before exclusions),",
        "scored_n, mae (mean absolute prediction error), signed_error (mean prediction minus truth),",
        "and coverage (fraction between lower and upper inclusive)."))),
    solution, "Score deliberately hidden measurements so each prediction can be compared with a known value.",
    if (level == 3L) "Filter eligible ids together with group_by(id) and all(). Summarise the same cases separately for each method."
    else "Use filter() to select masked measurements, then summarise() absolute errors and the requested diagnostics.",
    "evaluation |> filter(masked, !is.na(truth)) |> ...",
    "These scores describe performance on originally observed values under the chosen masking design. Accuracy and interval coverage for genuinely missing values remain unknown and may differ if the missingness process differs.",
    exercise_table(), function(ctx, variant) {
      offsets <- c(Harbor = sample(10:20, 1), Valley = sample(25:40, 1))
      training <- tidyr::expand_grid(site = names(offsets), noise = c(-4L, -2L, -1L, 1L, 2L, 4L)) |>
        dplyr::mutate(sensor = sample(30:60, dplyr::n()), score = sensor + unname(offsets[site]) + noise)
      local <- training |> dplyr::group_by(site) |>
        dplyr::summarise(correction = mean(score - sensor), .groups = "drop")
      pooled <- mean(training$score - training$sensor)
      cases <- tibble::tibble(id = sample(100:999, 24), site = rep(names(offsets), each = 12),
        sensor = sample(12:60, 24), masked = rep(c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE), 4))
      # Predictions use separate training measurements and the test sensor value.
      evaluation <- tidyr::expand_grid(cases, method = c("local", "pooled")) |>
        dplyr::left_join(local, by = "site") |>
        dplyr::mutate(prediction = sensor + dplyr::if_else(method == "local", correction, pooled),
                      lower = prediction - 2, upper = prediction + 2) |>
        dplyr::select(-correction, -sensor)
      noise <- sample(c(-5L, -2L, 0L, 2L, 4L), nrow(cases), TRUE)
      noise[2] <- -2L
      cases$truth <- cases$sensor + unname(offsets[cases$site]) + noise
      cases$truth[c(6, 18)] <- NA_integer_
      evaluation <- dplyr::left_join(evaluation, dplyr::select(cases, id, truth), by = "id")
      # One method has no row; a different id has a present but incomplete row.
      evaluation <- dplyr::filter(evaluation, !(id == cases$id[7] & method == "pooled"))
      evaluation$prediction[evaluation$id == cases$id[1] & evaluation$method == "pooled"] <- NA_integer_
      evaluation$lower[evaluation$id == cases$id[13] & evaluation$method == "local"] <- NA_integer_
      if (variant == 3L) evaluation$upper[evaluation$id == cases$id[4] & evaluation$method == "pooled"] <- NA_integer_
      list(evaluation = dplyr::slice_sample(evaluation, prop = 1))
    })
}

exercise_library_missing_resolution <- function() {
  category <- "Missing Data"
  list(
    missing_source_reconcile = exercise_template(category, "Reconcile competing source records", 1, resolution_sources),
    missing_identity = exercise_template(category, "Reconstruct counts from accounting identities", 1, resolution_identity),
    missing_eligible = exercise_template(category, "Audit complete-case eligibility and attrition", 1, resolution_eligibility),
    missing_carry = exercise_template(category, "Carry settings within valid time windows", 2, resolution_carry),
    missing_observation_weight = exercise_template(category, "Weight outcomes by observation probability", 2, resolution_weight),
    missing_delta_grid = exercise_template(category, "Find a missing-outcome tipping point", 3, resolution_delta),
    missing_binary_bounds = exercise_template(category, "Bound an incomplete binary outcome", 2, resolution_bounds),
    missing_compare_reports = exercise_template(category, "Report uncertainty across analysis methods", 3, resolution_report),
    missing_train_only = exercise_template(category, "Fit imputation rules on training data", 2, resolution_train_only),
    missing_mask_validation = exercise_template(category, "Validate predictions on masked measurements", 2, resolution_mask_validation)
  )
}
