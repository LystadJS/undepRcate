response_overlap <- function(ctx, level) {
  keys <- if (level == 1L) "site" else "c(site, risk)"
  rates <- sprintf(paste0("data |>\n  summarise(eligible = n(), observed = sum(!is.na(%s)), ",
    ".by = %s) |>\n  mutate(response_rate = observed / eligible)"), ctx$x, keys)
  solution <- switch(level, rates, paste0(rates, " |>\n  mutate(no_respondents = observed == 0)"),
    paste0("rates <- ", rates, "\ntarget |>\n  left_join(rates, by = c(\"site\", \"risk\")) |>\n",
           "  mutate(unsupported = if_else(coalesce(observed, 0L) == 0L, target_count, 0L)) |>\n",
           "  summarise(unsupported_share = sum(unsupported) / sum(target_count))"))
  task <- switch(level,
    sprintf("Every row in data is eligible. For each site return site, eligible (row count), observed (nonmissing %s count), and response_rate (observed divided by eligible).", ctx$x),
    sprintf("Every row in data is eligible. For each observed site and risk combination, return the keys, eligible (row count), observed (nonmissing %s count), response_rate and logical no_respondents. Retain strata with zero responses.", ctx$x),
    sprintf("target gives target_count people per site and risk stratum. Using the eligible sample data, return a one-row table with unsupported_share, the share of all target people whose stratum supplies no observed %s. Include target strata absent from the sample.", ctx$x))
  exercise_plan(task, solution,
    "Count the eligible people before restricting to respondents. A target stratum can lack respondent information even when it appears in the sample.",
    "Use summarise() by the requested keys. For the target comparison, start with target and left_join() the observed counts.",
    sprintf("data |> summarise(eligible = n(), observed = sum(!is.na(%s)), .by = ...)", ctx$x),
    "These observed rates diagnose sparse or absent support. A sample cell with no respondents supplies no direct outcome evidence for that cell. Its underlying response probability could still be positive.",
    exercise_table(), function(ctx, variant) {
      size <- sample(5:12, 1)
      data <- tidyr::expand_grid(site = c("Bay", "Hill"), risk = c("lower", "higher"), person = seq_len(size))
      score <- sample(0:50, nrow(data), TRUE)
      score[data$site == "Hill" & data$risk == "higher"] <- NA_integer_
      score[2:3] <- NA_integer_
      data[[ctx$x]] <- score
      target <- tidyr::expand_grid(site = c("Bay", "Hill", "New"), risk = c("lower", "higher"))
      target$target_count <- sample(4:95, nrow(target))
      list(data = dplyr::slice_sample(data, prop = 1), target = target)
    })
}

followup_gaps <- function(ctx, level) {
  last_seen <- sprintf(paste0("last_seen <- visits |>\n  filter(due, !is.na(%s)) |>\n",
    "  summarise(last_observed = max(visit), .by = id)"), ctx$x)
  timeline <- paste0("visits |>\n  filter(due) |>\n  left_join(last_seen, by = \"id\")")
  states <- sprintf(paste0("mutate(state = case_when(\n    !is.na(%s) ~ \"observed\",\n",
    "    is.na(last_observed) ~ \"never observed\",\n    visit < last_observed ~ \"intermittent\",\n",
    "    TRUE ~ \"trailing\"))"), ctx$x)
  body <- switch(level,
    paste0(timeline, " |>\n  distinct(id, last_observed)"),
    paste0(timeline, " |>\n  ", states, " |>\n  select(id, visit, state)"),
    paste0(timeline, " |>\n  ", states, " |>\n  summarise(intermittent = sum(state == \"intermittent\"),\n",
           "            trailing = sum(state == \"trailing\"), never_observed = all(state == \"never observed\"), .by = id)"))
  task <- paste0("In visits, assess only scheduled rows (due is TRUE). A visit is observed when ", ctx$x,
    " is recorded. ",
    switch(level,
      "Return id and last_observed, the largest observed scheduled visit for each person with any scheduled row. Use missing for a person never observed.",
      paste0("Return id, visit and state per scheduled row. Use 'observed' for a recorded value, 'never observed' if the person has none, ",
             "'intermittent' for a gap before a later observation and 'trailing' for a gap after the final observation."),
      paste0("Return id, intermittent (number of missed visits before a later observation), trailing (number after the final observation), ",
             "and never_observed (logical). For a person never observed, both counts are zero.")))
  exercise_plan(task, paste(last_seen, body, sep = "\n"), "Reconstruct each person's scheduled timeline before classifying a gap.",
    "Use filter() and summarise() for the final observed visit per person. Join it to all scheduled rows.",
    sprintf("visits |> filter(due, !is.na(%s)) |> summarise(last_observed = max(visit), .by = id)", ctx$x),
    "A later observed visit identifies an intermittent gap. A run of missing values at the end is consistent with dropout, but the timeline alone does not establish why follow-up ended.",
    exercise_table(), function(ctx, variant) {
      ids <- sample(100:999, 5L)
      visits <- tidyr::expand_grid(id = ids, visit = seq_len(sample(5:8, 1L)))
      visits$due <- !(visits$id == ids[4] & visits$visit > 3)
      score <- sample(0:20, nrow(visits), TRUE)
      score[visits$id == ids[1] | (visits$id == ids[2] & visits$visit > 2) |
              (visits$id == ids[3] & visits$visit == 2)] <- NA_integer_
      visits[[ctx$x]] <- score
      list(visits = dplyr::slice_sample(visits, prop = 1))
    })
}

exercise_library_missing_followup <- function() {
  list(md_followup = exercise_template("Missing Data", "Distinguish intermittent and trailing gaps", 2L, followup_gaps),
       md_overlap = exercise_template("Missing Data", "Assess response rates and target coverage", 2L, response_overlap))
}
