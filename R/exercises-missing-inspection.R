# Reference code is graded against the preview and two independently generated
# fixtures. Structural skips are introduced only where eligibility is provided.
missing_wide_fixture <- function(ctx, variant) {
  fields <- c(ctx$x, ctx$y, "rating")
  size <- sample(13:27, 1L)
  df <- data.frame(record_id = sample(100:999, size), site = rep(c("Bay", "Glen"), length.out = size),
                   wave = sample(1:4, size, TRUE), note = sample(c("ok", NA), size, TRUE))
  for (field in fields) {
    values <- sample(c(0:16, NA_real_, NA_real_), size, TRUE)
    df[[field]] <- values
  }
  df[1:4, fields] <- rbind(c(0, 4, 8), c(NA, NA, NA), c(NA, 5, NA), c(7, NA, 5))
  if (variant == 3L) df[df$site == "Glen", fields[1]] <- 1
  list(df = df[sample.int(size), ], fields = fields)
}

missing_eligibility_fixture <- function(ctx, variant) {
  size <- sample(25:45, 1L)
  df <- data.frame(site = rep(c("Bay", "Glen"), length.out = size),
                   wave = sample(1:4, size, TRUE), field = sample(c(ctx$x, ctx$y), size, TRUE),
                   due = sample(c(TRUE, TRUE, FALSE, NA), size, TRUE),
                   value = sample(c(0:12, rep(NA_real_, 5)), size, TRUE))
  df$due[1:5] <- c(TRUE, TRUE, FALSE, FALSE, NA)
  df$value[1:5] <- c(0, NA, NA, 8, NA)
  # The extra site supplies an observed stratum with no eligible records.
  df <- rbind(df, data.frame(site = "Ridge", wave = 1:2, field = ctx$x,
                             due = c(FALSE, NA), value = c(NA_real_, 3)))
  list(df = df[sample.int(nrow(df)), ])
}

missing_csv_fixture <- function(ctx, variant) {
  make_csv <- function() {
    ids <- sprintf("%04d", sample(1:999, 6))
    visit <- sample(1:5, 6, TRUE)
    raw <- c("0", "MISS", "", as.character(sample(1:49, 3) + c(0, .25, .5)))
    notes <- c('"review, later"', "NA", '""', '"a ""quote"""', "OK", "MISS")
    paste(c("person,visit,reading,note", paste(ids, visit, raw, notes, sep = ",")), collapse = "\n")
  }
  list(csv = make_csv(), feeds = setNames(list(make_csv(), make_csv()), sample(c("east", "west", "north"), 2)))
}

missing_intake <- function(ctx, level) {
  import <- 'readr::read_csv(I(csv), col_types = readr::cols(.default = readr::col_character()), na = character(), trim_ws = FALSE)'
  convert <- 'mutate(visit = readr::parse_integer(visit),\n         reading = readr::parse_double(reading, na = c("", "MISS")))'
  task <- "Import the CSV text in csv as four character columns. Preserve leading zeroes, empty strings, literal missing-value tokens, and punctuation inside quoted fields. Return every record."
  code <- import
  if (level >= 2L) {
    task <- paste(task, "Then convert visit to integer and reading to double. Only an empty reading or the reading token MISS denotes a missing number. Keep note and person as supplied.")
    code <- paste(import, convert, sep = " |>\n  ")
  }
  if (level == 3L) {
    task <- "Import each CSV text in the named list feeds. Bind the rows with a character source column giving the feed name. Keep person and note exactly as supplied, including leading zeroes, literal tokens, and quoted punctuation. Return visit as integer and reading as double, treating only empty readings and MISS as missing."
    read_one <- sub("I(csv)", "I(text)", import, fixed = TRUE)
    code <- paste0("feeds |>\n  map(function(text) ", read_one, ") |>\n  list_rbind(names_to = \"source\") |>\n  ", convert)
  }
  exercise_plan(task, code, "An identifier, a note, and a measurement can give the same token different meanings.",
                "Use readr::read_csv() on I(text) with an explicit character column specification. Apply field-specific conversions after import.",
                if (level == 3L) 'feeds |> map(function(text) ...) |> list_rbind(names_to = "source")' else 'readr::read_csv(I(csv), col_types = ..., na = character())',
                "Reading raw fields as text keeps the original identifiers and tokens available. The codebook determines which measurement values represent missing information.",
                if (level == 1L) exercise_table() else exercise_table(storage = c(visit = "integer", reading = "double")),
                missing_csv_fixture)
}

missing_sentinels <- function(ctx, level) {
  refused <- sample(c(-91L, -97L, -991L), 1)
  skipped <- sample(c(-88L, -98L, -998L), 1)
  reason <- sprintf('case_when(raw == %d ~ "refused", raw == %d ~ "not applicable", is.na(raw) ~ "unrecorded", TRUE ~ "observed")', refused, skipped)
  code <- sprintf("df |> mutate(clean = if_else(raw %%in%% c(%d, %d), NA_real_, raw))", refused, skipped)
  task <- sprintf("Keep every row and all original columns of df. Add numeric clean. Recode only %d (refused) and %d (not applicable) in raw to missing, preserving legitimate zeros, other negative values and raw itself.", refused, skipped)
  if (level >= 2L) {
    task <- paste(task, 'Also add reason with the character labels "refused", "not applicable", "unrecorded" (original NA), and "observed".')
    code <- paste0(code, " |>\n  mutate(reason = ", reason, ")")
  }
  if (level == 3L) {
    task <- paste(task, 'Add logical eligible (false only for "not applicable") and integer observed (one for a recorded eligible value, zero for an eligible missing value, NA when ineligible).')
    code <- paste0(code, ' |>\n  mutate(eligible = reason != "not applicable",\n         observed = if_else(eligible, as.integer(!is.na(clean)), NA_integer_))')
  }
  exercise_plan(task, code, "Keep the source codes until the reason for each empty measurement has been recorded.",
                "Use mutate() with explicit codebook matches. case_when() distinguishes reasons that collapse to the same cleaned NA.",
                sprintf("df |> mutate(clean = if_else(raw %%in%% c(%d, %d), ..., raw))", refused, skipped),
                "Structural ineligibility removes an opportunity to observe the measure. Refusal and nonrecording leave an eligible measurement missing.",
                exercise_table(TRUE, storage = if (level == 3L) c(observed = "integer")), function(ctx, variant) {
                  size <- sample(10:22, 1)
                  raw <- c(0, -1, refused, skipped, NA_real_, sample(c(-20:30, refused, skipped), size, TRUE))
                  list(df = data.frame(id = sample(100:999, length(raw)), raw = sample(raw)))
                })
}

missing_by_field <- function(ctx, level) {
  keys <- if (level == 3L) "c(site, field)" else "field"
  add <- if (level >= 2L) ", records = n(), rate = missing / records" else ""
  task <- "For the measurement names listed in fields, report one row per field with field and missing (the number of NA values). Each source row represents one eligible record."
  if (level >= 2L) task <- paste(task, "Include records (all records) and rate (the missing fraction).")
  if (level == 3L) task <- paste(task, "Report these summaries separately within each site, including site in the output.")
  code <- sprintf('df |>\n  pivot_longer(all_of(fields), names_to = "field", values_to = "value") |>\n  summarise(missing = sum(is.na(value))%s, .by = %s)', add, keys)
  exercise_plan(task, code, "Count only measurement fields. Missing notes and identifiers are outside this audit.",
                "Stack the selected fields with pivot_longer(), then summarise the missing indicators.",
                'df |> pivot_longer(all_of(fields), names_to = "field", values_to = "value") |> ...',
                "A long table gives each eligible record one entry per requested field, so its missing indicators supply the numerator and its row count supplies the denominator.",
                exercise_table(), missing_wide_fixture)
}

missing_by_record <- function(ctx, level) {
  task <- "For each record_id in df, count missing measurements among fields only. Return record_id and missing. Every listed field is eligible."
  summaries <- "missing = sum(is.na(value))"
  if (level >= 2L) {
    task <- paste(task, "Add rate as the fraction of listed fields missing and status as complete, partial, or empty (all fields missing).")
    summaries <- paste0(summaries, ', rate = missing / n(),\n            status = case_when(missing == 0 ~ "complete", missing == n() ~ "empty", TRUE ~ "partial")')
  }
  if (level == 3L) {
    task <- paste(task, "Add absent_fields listing the missing field names alphabetically, separated by comma and space. Use an empty string for a complete record.")
    summaries <- paste0(summaries, ',\n            absent_fields = paste(sort(field[is.na(value)]), collapse = ", ")')
  }
  code <- sprintf('df |>\n  pivot_longer(all_of(fields), names_to = "field", values_to = "value") |>\n  summarise(%s, .by = record_id)', summaries)
  exercise_plan(task, code, "Summarize within a person or record instead of down a column.",
                "Reshape the measurement fields with pivot_longer() and use summarise(.by = record_id).",
                'df |> pivot_longer(all_of(fields), names_to = "field", values_to = "value") |> ...',
                "This denominator counts the requested fields for one record. Unrelated metadata has no bearing on the record's completeness.",
                exercise_table(), missing_wide_fixture)
}

missing_eligible <- function(ctx, level) {
  keys <- if (level >= 2L) "c(site, field)" else "field"
  counts <- "eligible = sum(due %in% TRUE), missing = sum(due %in% TRUE & is.na(value))"
  task <- "Summarize df by field. Return eligible (count of rows where due is true), missing (eligible rows with NA value), and rate = missing divided by eligible. Use a numeric NA when there are no eligible rows. Keep groups with zero eligible rows."
  if (level >= 2L) {
    task <- paste(task, "Split by site as well and include unknown_due, the number of rows with unknown eligibility.")
    counts <- paste(counts, "unknown_due = sum(is.na(due))", sep = ",\n            ")
  }
  if (level == 3L) {
    task <- paste(task, "Add structural (all explicitly ineligible rows) and unexpected (a recorded value despite explicit ineligibility).")
    counts <- paste(counts, "structural = sum(due %in% FALSE), unexpected = sum(due %in% FALSE & !is.na(value))", sep = ",\n            ")
  }
  code <- sprintf("df |>\n  summarise(%s,\n            rate = if_else(eligible > 0, missing / eligible, NA_real_), .by = %s)", counts, keys)
  exercise_plan(task, code, "Define who could have supplied the measurement before counting its absence.",
                "Use explicit membership in TRUE or FALSE to keep unknown eligibility separate. Use if_else() for an undefined rate.",
                "df |> summarise(eligible = sum(due %in% TRUE), ...)",
                "The risk set contains confirmed eligible rows. Retaining zero-eligible groups makes undefined rates visible.",
                exercise_table(), missing_eligibility_fixture)
}

missing_patterns <- function(ctx, level) {
  vars <- c(ctx$x, ctx$y, if (level >= 2L) "rating")
  cols <- paste(vars, collapse = ", ")
  task <- sprintf("Count joint response patterns across %s, in precisely that order. Encode each row as a character string of digits with 1 for observed and 0 for missing. Return pattern and records for observed patterns only.", cols)
  code <- sprintf('df |>\n  mutate(across(c(%s), ~ as.integer(!is.na(.x)))) |>\n  unite(pattern, %s, sep = "")', cols, cols)
  if (level == 3L) {
    task <- paste(task, "Count separately within each site and add share as the fraction of that site's records with the pattern.")
    code <- paste0(code, ' |>\n  count(site, pattern, name = "records") |>\n  mutate(share = records / sum(records), .by = site)')
  } else {
    code <- paste0(code, ' |>\n  count(pattern, name = "records")')
  }
  exercise_plan(task, code, "Two datasets with the same per-field missing counts can have different joint patterns.",
                "Use across() to encode the observation indicators and unite() in the prescribed field order before counting.",
                sprintf("df |> mutate(across(c(%s), ~ as.integer(!is.na(.x)))) |> ...", cols),
                "The string retains which measurements are observed together. Per-field totals discard this information.",
                exercise_table(), missing_wide_fixture)
}

missing_coabsence <- function(ctx, level) {
  both <- sprintf("sum(is.na(%s) & is.na(%s))", ctx$x, ctx$y)
  expr <- sprintf("both = %s", both)
  task <- sprintf("Return a one-row data frame with both, the number of records where both %s and %s are missing.", ctx$x, ctx$y)
  if (level >= 2L) {
    expr <- paste0(expr, sprintf(",\n            first_absent = sum(is.na(%s)), conditional_rate = if_else(first_absent > 0, both / first_absent, NA_real_)", ctx$x))
    task <- paste(task, sprintf("Include first_absent (rows missing %s) and conditional_rate (fraction of those rows also missing %s, NA if none).", ctx$x, ctx$y))
  }
  if (level == 3L) {
    task <- sub("a one-row data frame", "one row per site", task, fixed = TRUE)
    task <- paste(task, "Include site and joint_rate, the share of all site records missing both fields.")
    expr <- paste0(expr, ",\n            joint_rate = both / n(), .by = site")
  }
  exercise_plan(task, sprintf("df |> summarise(%s)", expr),
                "Joint absence needs both conditions on the same record.",
                "Combine is.na() indicators with &, then sum them inside summarise().",
                sprintf("df |> summarise(both = sum(is.na(%s) & ...), ...)", ctx$x),
                "A joint rate divides by all records. A conditional rate restricts the denominator to records already missing the first field.",
                exercise_table(), missing_wide_fixture)
}

missing_visits <- function(ctx, level) {
  task <- "schedule lists the visits actually due for each person. Return person and week for scheduled visits with no row in visits. An attended visit can have a missing reading. Ignore unscheduled visits."
  code <- 'schedule |> anti_join(visits, by = c("person", "week")) |> select(person, week)'
  if (level >= 2L) {
    task <- 'Return every planned visit in schedule with person, week, site and a character status. Use "no visit" for an absent key, "no reading" for an attended visit without a recorded reading, and "recorded" for a visit with a reading. Ignore unscheduled visits.'
    code <- paste0(
      'attendance <- visits |>\n  summarise(any_reading = any(!is.na(reading)), .by = c(person, week))\n',
      'schedule |>\n  left_join(attendance, by = c("person", "week")) |>\n',
      '  mutate(status = case_when(is.na(any_reading) ~ "no visit", any_reading ~ "recorded", TRUE ~ "no reading")) |>\n',
      '  select(person, week, site, status)'
    )
  }
  if (level == 3L) {
    task <- 'Audit visits against the individual schedule. A planned visit may have duplicate submissions. Count it as recorded if any submission contains a reading. Return one row per site with planned (scheduled keys), absent (keys with no visit), unread (attended keys with no recorded reading), and unavailable_rate (absent plus unread divided by planned). Exclude unscheduled visits.'
    code <- paste0(code, ' |>\n  summarise(planned = n(), absent = sum(status == "no visit"), unread = sum(status == "no reading"),\n            unavailable_rate = (absent + unread) / planned, .by = site)')
  }
  exercise_plan(task, code, "A missing row and a missing value inside an existing row are two different observations.",
                "Join on both person and week. Aggregate submissions for each key before counting scheduled visits.",
                if (level == 1L) 'schedule |> anti_join(visits, by = ...)' else 'visits |> summarise(any_reading = any(!is.na(reading)), .by = c(person, week))',
                "The individual schedule supplies the denominator. The attendance summary distinguishes a nonexistent visit from an existing visit whose measurement is missing.",
                exercise_table(), function(ctx, variant) {
                  people <- sample(101:799, sample(5:8, 1))
                  schedule <- expand.grid(person = people, week = c(0L, 2L, 6L))
                  schedule <- schedule[-sample(seq_len(nrow(schedule)), 3), ]
                  schedule$site <- ifelse(match(schedule$person, people) %% 2L == 0, "Hill", "Bay")
                  positions <- sample(seq_len(nrow(schedule)), nrow(schedule) - 3L)
                  visits <- schedule[positions, c("person", "week")]
                  visits$reading <- sample(3:16, nrow(visits), TRUE)
                  visits$reading[1:2] <- NA_real_
                  if (level == 3L) {
                    extra <- visits[1:2, ]
                    extra$reading <- c(7, NA_real_)
                    visits <- rbind(visits, extra)
                  }
                  visits <- rbind(visits, data.frame(person = people[1], week = 99L, reading = 26))
                  list(schedule = schedule, visits = visits[sample.int(nrow(visits)), ])
                })
}

missing_join_gaps <- function(ctx, level) {
  task <- "The incoming table records contains a person key, which must be nonmissing to be usable. Return row_id and a logical incomplete_key for each row."
  code <- "records |> transmute(row_id, incomplete_key = is.na(person))"
  if (level >= 2L) {
    keys <- if (level == 2L) '"person"' else 'c("site", "person")'
    incomplete <- if (level == 2L) "is.na(person)" else "is.na(site) | is.na(person)"
    task <- sprintf('Match records to roster on %s. Return row_id and status with exactly one of the following character labels: "incomplete key" if a required key component is missing, "unmatched" if the complete key has no roster match, "missing value" if the matched roster row lacks its value, or "recorded". Missing keys never match.', if (level == 2L) "person" else "both site and person")
    code <- sprintf('records |>\n  left_join(roster |> mutate(found = TRUE), by = %s, na_matches = "never") |>\n  transmute(row_id, status = case_when(%s ~ "incomplete key", is.na(found) ~ "unmatched",\n                                       is.na(value) ~ "missing value", TRUE ~ "recorded"))', keys, incomplete)
  }
  exercise_plan(task, code, "A missing value after a join can have several origins. Keep evidence of whether the right-hand record existed.",
                "Use an explicit completeness check on the key. Give roster a presence flag before a left_join().",
                if (level == 1L) "records |> transmute(row_id, incomplete_key = ...)" else 'roster |> mutate(found = TRUE)',
                "The key, a roster-presence flag and the roster value answer three different questions. Checking them in that order preserves the reason.",
                exercise_table(), function(ctx, variant) {
                  people <- sample(101:999, 5)
                  records <- data.frame(row_id = sample(1:99, 8), person = people[c(1, 2, 3, 4, 1, 2, 3, 5)],
                                        site = c("A", "A", "A", "B", "B", "A", "B", "B"))
                  records$person[6] <- NA_integer_
                  records$site[8] <- NA_character_
                  roster <- data.frame(person = c(people[1:3], NA_integer_), value = c(1, NA_real_, 0, 2))
                  if (level == 3L) {
                    roster$site <- "A"
                    roster <- rbind(roster, data.frame(person = people[1], value = NA_real_, site = "B"))
                  }
                  list(records = records[sample.int(nrow(records)), ], roster = roster)
                })
}

missing_rate_plot <- function(ctx, level) {
  keys <- switch(level, "wave", "c(wave, site)", "c(wave, site, field)")
  colour <- if (level >= 2L) ", colour = site" else ""
  facet <- if (level == 3L) " + facet_wrap(vars(field))" else ""
  task <- paste("Among eligible rows of df (due is true), calculate the fraction of missing values at each wave.",
                "Plot the numeric fraction on the vertical axis using fixed scale limits of zero and one. Put numeric wave on the horizontal axis and use one default point layer. Omit groups with no eligible rows.")
  if (level >= 2L) task <- paste(task, "Calculate rates separately by site and map colour to site.")
  if (level == 3L) task <- paste(task, "Compute separate rates for each field and use one wrapped panel per field with shared axes.")
  code <- sprintf('rates <- df |>\n  filter(due %%in%% TRUE) |>\n  summarise(rate = mean(is.na(value)), .by = %s)\nggplot(rates, aes(x = wave, y = rate%s)) + geom_point() + scale_y_continuous(limits = c(0, 1))%s', keys, colour, facet)
  exercise_plan(task, code, "Plot a rate within a comparable set of eligible observations.",
                "Filter to eligible records, then compute mean(is.na(value)) inside each plotting group.",
                "rates <- df |> filter(due %in% TRUE) |> summarise(rate = ..., .by = ...)",
                "Comparisons across time and recorded groups can suggest follow-up questions about collection. These observed comparisons alone cannot identify dependence on the missing values.",
                list(kind = "plot", default_style = TRUE), function(ctx, variant) {
                  data <- missing_eligibility_fixture(ctx, variant)
                  if (variant == 3L) {
                    # Every eligible plotting group has observed and missing
                    # measurements, so automatically trained scales differ from 0..1.
                    paired <- data$df[data$df$due %in% TRUE, ]
                    paired$value <- ifelse(is.na(paired$value), 0, NA_real_)
                    data$df <- rbind(data$df, paired)
                  }
                  data
                })
}

missing_mechanism_rules <- function(ctx, level) {
  task <- "rules is a deliberately specified probability law for one optional score. The stratum is always observed. The score would be unknown on a missed measurement. For each protocol return protocol and constant, a logical flag for equal miss_prob throughout the full stratum-by-score grid."
  code <- "rules |> summarise(constant = n_distinct(miss_prob) == 1, .by = protocol)"
  if (level >= 2L) {
    task <- paste(task, "Also return residual_dependence, true if changing score within at least one fixed stratum changes miss_prob.")
    code <- paste0('rules |>\n  summarise(low = min(miss_prob), high = max(miss_prob), .by = c(protocol, stratum)) |>\n',
                   '  summarise(constant = min(low) == max(high), residual_dependence = any(low != high), .by = protocol)')
  }
  if (level == 3L) {
    task <- paste(task, 'Add mechanism using the most restrictive label consistent with this supplied law: "MCAR" for a constant probability, "MAR" for a varying probability that depends only on the observed stratum, and "MNAR" for remaining dependence on the potentially missing score.')
    code <- paste0(code, ' |>\n  mutate(mechanism = case_when(constant ~ "MCAR", residual_dependence ~ "MNAR", TRUE ~ "MAR"))')
  }
  exercise_plan(task, code, "This exercise supplies the entire mechanism, including probabilities at scores that would be absent in real records.",
                "Compare the range of miss_prob both across the grid and within each observed stratum.",
                if (level == 1L) "rules |> summarise(constant = n_distinct(...) == 1, .by = protocol)" else "rules |> summarise(low = min(miss_prob), high = max(miss_prob), .by = c(protocol, stratum)) |> ...",
                "For this single-score missing pattern, MAR holds when the missing-pattern probability stays fixed across possible missing scores at the same observed stratum. A constant probability satisfies MCAR as well. These exact comparisons use a stipulated law, not estimated sample rates. Ignorability requires additional conditions for the chosen inference.",
                exercise_table(), function(ctx, variant) {
                  protocols <- sample(LETTERS, 3)
                  rules <- expand.grid(protocol = protocols, stratum = c("East", "West"), score = 0:1, stringsAsFactors = FALSE)
                  intercept <- sample(1:3, 1L)
                  step <- sample(2:3, 1L)
                  points <- intercept + (rules$protocol != protocols[1]) * (rules$stratum == "East") * step +
                    (rules$protocol == protocols[3]) * rules$score
                  rules$miss_prob <- points / 10
                  list(rules = rules[sample.int(nrow(rules)), ])
                })
}

missing_observed_equivalence <- function(ctx, level) {
  task <- "cells gives frequencies under two hypothetical complete-data laws. recorded is true when the binary outcome is available. For every scenario, replace outcome by NA on unrecorded cells, pool identical observed states, and return scenario, recorded, outcome and records (summed frequency)."
  code <- 'cells |>\n  mutate(outcome = if_else(recorded, outcome, NA_integer_)) |>\n  summarise(records = sum(freq), .by = c(scenario, recorded, outcome))'
  if (level >= 2L) {
    task <- "cells gives two hypothetical complete-data frequency tables with a binary outcome. Summarize each scenario using only what would be recorded. Return scenario, response_rate (fraction recorded) and observed_mean (fraction of recorded outcomes equal to one). Weight every cell by freq."
    code <- 'cells |>\n  summarise(response_rate = sum(freq[recorded]) / sum(freq),\n            observed_mean = sum(freq[recorded] * outcome[recorded]) / sum(freq[recorded]), .by = scenario)'
  }
  if (level == 3L) {
    task <- paste(task, "For sensitivity analysis add full_mean (mean over the entire hypothesized complete table) and gap (full_mean minus observed_mean).")
    code <- sub(", .by = scenario)", ",\n            full_mean = sum(freq * outcome) / sum(freq), gap = full_mean - observed_mean, .by = scenario)", code, fixed = TRUE)
  }
  exercise_plan(task, code, "A frequency-table row represents freq people. Only recorded outcomes contribute to an observed outcome summary.",
                "Use mutate() to mask unrecorded cells or summarise() to aggregate their frequencies at the required level.",
                if (level == 1L) 'cells |> mutate(outcome = if_else(recorded, outcome, NA_integer_)) |> ...'
                else 'cells |> summarise(response_rate = sum(freq[recorded]) / sum(freq), ...)',
                "The two laws have identical observed information but allocate the missing outcomes differently. Their complete-data means can therefore differ. Observed agreement alone cannot distinguish MAR from MNAR.",
                exercise_table(), function(ctx, variant) {
                  scenarios <- sample(c("amber", "blue", "sage", "umber"), 2)
                  counts <- sample(4:12, 2)
                  shift <- sample(seq_len(min(counts) - 1L), 1L)
                  cells <- expand.grid(outcome = 0:1, recorded = c(FALSE, TRUE), scenario = scenarios, stringsAsFactors = FALSE)
                  cells$freq <- counts[cells$outcome + 1L]
                  move <- !cells$recorded & cells$scenario == scenarios[2L]
                  cells$freq[move] <- cells$freq[move] + ifelse(cells$outcome[move] == 1L, shift, -shift)
                  list(cells = cells[sample.int(nrow(cells)), ])
                })
}

exercise_library_missing_inspection <- function() {
  category <- "Missing Data"
  list(
    md_intake = exercise_template(category, "Import records without losing missing codes", 1, missing_intake),
    md_sentinels = exercise_template(category, "Recode sentinels and preserve reasons", 1, missing_sentinels),
    md_field_profile = exercise_template(category, "Profile missing measurements by field", 1, missing_by_field),
    md_record_profile = exercise_template(category, "Profile incomplete records", 1, missing_by_record),
    md_eligible = exercise_template(category, "Set the eligible denominator", 2, missing_eligible),
    md_patterns = exercise_template(category, "Count joint response patterns", 2, missing_patterns),
    md_coabsence = exercise_template(category, "Measure pairwise co-absence", 2, missing_coabsence),
    md_visits = exercise_template(category, "Find absent visits on a schedule", 2, missing_visits),
    md_join_gaps = exercise_template(category, "Separate join failures from missing values", 2, missing_join_gaps),
    md_rate_plot = exercise_template(category, "Plot missingness by wave and site", 2, missing_rate_plot),
    md_mechanism_rules = exercise_template(category, "Check a stipulated response mechanism", 3, missing_mechanism_rules),
    md_equivalence = exercise_template(category, "Compare observationally equivalent scenarios", 3, missing_observed_equivalence)
  )
}
