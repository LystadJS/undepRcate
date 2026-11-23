# Guided lessons use only the displayed worked fixture. Their summaries never
# evaluate a submission or inspect the two hidden grading fixtures.
missing_symbol <- function(symbol, meaning, conditioning, status = "Deterministic summary",
                           shape = "Scalar, 1 × 1") {
  c(symbol, meaning, shape, status, conditioning)
}

missing_lesson <- function(intuition, steps, math, ...) {
  guided_notes(intuition, steps, math, Filter(Negate(is.null), list(...)))
}

missing_number <- function(x) format(round(x, 3), trim = TRUE, scientific = FALSE)

guided_missing_notes <- function(ex) {
  if (startsWith(ex$id, "mice_")) return(guided_missing_imputation(ex))
  if (startsWith(ex$id, "missing_")) return(guided_missing_resolution(ex))
  if (startsWith(ex$id, "md_")) return(guided_missing_inspection(ex))
  NULL
}

guided_missing_inspection <- function(ex) {
  lvl <- ex$level
  input <- ex$inputs
  df <- input$df
  s <- missing_symbol
  switch(ex$id,
    md_intake = {
      text <- if (lvl == 3L) input$feeds[[1L]] else input$csv
      first_id <- substr(strsplit(text, "\n", fixed = TRUE)[[1L]][2L], 1L, 4L)
      missing_lesson(
        "CSV carries text. The field's codebook determines whether a token is an identifier, a number or a missing value.",
        c(sprintf("Read all four columns as character with automatic missing-token conversion disabled. Keep all four characters of the first identifier, '%s'.", first_id),
          if (lvl == 1L) "Let the CSV reader handle quoted commas and escaped quotes. Inspect the literal MISS in reading and the literal NA in note."
          else "Convert visit with parse_integer() and reading with parse_double(). Give the reading parser its own empty-string and MISS rules, so literal notes survive.",
          if (lvl == 3L) "Map the same reader over feeds, then bind rows with names_to so each source name stays attached."),
        if (lvl == 1L) "h('MISS') = 'MISS'" else "h('MISS') = NA     h('0') = 0",
        s("h", if (lvl == 1L) "Raw reading-field import in read_csv()." else "Field-specific reading conversion in parse_double().",
          "Given the CSV quoting rules and the specified reading codebook.", "Deterministic function",
          if (lvl == 1L) "CSV field token → character value" else "Reading token → double value or missing double"),
        if (lvl > 1L) s("NA", "R's missing-value marker for the parsed reading.", "Given a designated missing token.", "Fixed code", "One missing double"))
    },
    md_sentinels = {
      codes <- regmatches(ex$instructions, gregexpr("-[0-9]+", ex$instructions))[[1L]][1:2]
      missing_lesson(
        "Two source codes can both become missing measurements while still telling different stories about why the value is absent.",
        c(sprintf("Keep raw. Recode exactly %s and %s in clean. The observed value −1 in this example remains −1.", codes[1], codes[2]),
          if (lvl == 1L) "Check a recorded zero and an original NA after recoding. Each should keep its original meaning."
          else "Derive reason from raw while refusal, structural ineligibility and original nonrecording are distinguishable.",
          if (lvl == 3L) "Set eligible from the structural reason. Among eligible records, encode a recorded measurement as integer one and a missing measurement as zero. Leave the indicator missing for ineligible records."),
        sprintf("h(%s) = NA     h(%s) = NA     h(−1) = −1", codes[1], codes[2]),
        s("h", "Recoding function implemented by if_else() for clean.", "Given the two codebook entries and original raw value.",
          "Deterministic function", "Raw numeric value or missing → cleaned numeric value or missing"),
        s("NA", "R's numeric missing-value marker in clean.", "Given the codebook or an originally missing raw value.", "Fixed code"))
    },
    md_field_profile = {
      field <- input$fields[1L]
      rows <- if (lvl == 3L) df$site == df$site[1L] else rep(TRUE, nrow(df))
      total <- sum(rows)
      absent <- sum(is.na(df[[field]][rows]))
      group <- if (lvl == 3L) paste("at", df$site[1L]) else "across the table"
      missing_lesson("Turn each requested field into its own set of observation opportunities. Metadata columns stay outside its denominator.",
        c(sprintf("Pivot just the names in fields. For %s %s, this yields %d eligible rows.", field, group, total),
          sprintf("Sum the missing flags within this group to count the %d rows with absent measurements.", absent),
          if (lvl >= 2L) "Keep the full group count, then divide the missing count by it."),
        sprintf("r = d / n = %d / %d ≈ %s", absent, total, missing_number(absent / total)),
        s("d", "Missing-cell count from sum(is.na(value)).", "Fixed field and, when requested, site."),
        s("n", "Number of sampled eligible records in the group, from n().", "Same field and group.", "Observed count"),
        s("r", "Realized missing fraction, missing / records.", "Fixed eligible records and their observation flags."))
    },
    md_record_profile = {
      flags <- is.na(df[1L, input$fields, drop = FALSE])
      missing_lesson("Turn the table sideways in your head. This time the question is how much was recorded for one person.",
        c(sprintf("Select the %d requested measurements and pivot them, keeping record_id. Record %s has a row for each requested field.", length(flags), df$record_id[1L]),
          sprintf("Within that record, count %d missing entries. A blank note outside fields contributes zero to the count.", sum(flags)),
          if (lvl > 1L) "Divide by the field count and distinguish empty, partial and complete records.",
          if (lvl == 3L) "Sort just the absent field names before joining them into one string. A complete record gets an empty string."),
        sprintf("r = d / v = %d / %d ≈ %s", sum(flags), length(flags), missing_number(mean(flags))),
        s("d", "Number of listed fields missing in this record, the missing output.", "Fixed record_id and requested fields."),
        s("v", "Number of requested measurements, length(fields).", "Given the selected exercise.", "Fixed design count"),
        s("r", "Realized missing fraction for the record, rate.", "Selected record and requested fields."))
    },
    md_eligible = {
      first <- which(df$due %in% TRUE)[1L]
      rows <- df$field == df$field[first]
      if (lvl > 1L) rows <- rows & df$site == df$site[first]
      due <- rows & df$due %in% TRUE
      d <- sum(due & is.na(df$value))
      n <- sum(due)
      missing_lesson("A blank can enter the missing-rate denominator only if the measurement was due.",
        c(sprintf("Within field %s%s, count the TRUE values of due. Here there are %d confirmed opportunities.", df$field[first],
                  if (lvl > 1L) paste0(" at ", df$site[first]) else "", n),
          sprintf("Among those opportunities %d values are blank. Summarise from the full input so a group with no eligible rows remains present with an undefined rate.", d),
          if (lvl >= 2L) "Count unknown eligibility separately. An unknown schedule cannot establish a missed measurement.",
          if (lvl == 3L) "Count explicitly ineligible rows, then flag any value recorded on such a row as unexpected."),
        sprintf("r = d / e = %d / %d ≈ %s", d, n, missing_number(d / n)),
        s("d", "Count of eligible rows with is.na(value), missing.", "Fixed field and requested site stratum."),
        s("e", "Confirmed opportunity count, eligible.", "Same group, with due equal to TRUE.", "Observed count"),
        s("r", "Missing fraction among confirmed opportunities, rate.", "Positive eligible count and fixed record statuses."))
    },
    md_visits = {
      schedule <- input$schedule
      visits <- input$visits
      absent <- dplyr::anti_join(schedule, visits, by = c("person", "week"))
      missing_lesson("The schedule tells you what could be absent. A visit table alone contains no row for a visit that never happened.",
        c(sprintf("Use person and week together. Of the %d planned keys in the schedule, %d have no visit row.", nrow(schedule), nrow(absent)),
          if (lvl == 1L) "Anti-join from schedule to visits, then select those two keys. An attended visit with an empty reading still matches."
          else "For each submitted key, determine whether any submission has a reading. Left-join the resulting flag onto the schedule and distinguish an absent key from FALSE.",
          if (lvl == 3L) "Summarise once per planned key within site. Add absent keys and attended-but-unread keys before dividing by the scheduled count."),
        "G = S ∖ V",
        s("S", "Distinct planned person–week keys in schedule.", "Given the individual schedules.", "Observed set",
          sprintf("Finite set of %d pairs from person IDs × scheduled weeks", nrow(schedule))),
        s("V", "Distinct submitted person–week keys in visits.", "Given the submitted visit records.", "Observed set",
          "Finite set of pairs from person IDs × visit weeks"),
        s("G", "Scheduled keys without a submission, the anti_join() result.", "Given both input key sets.", "Deterministic set",
          sprintf("Finite set of %d person–week pairs", nrow(absent))))
    },
    md_join_gaps = {
      record <- input$records
      missing_lesson("A missing lookup result can originate in the key, the match or the matched value. Preserve evidence of each stage.",
        c(sprintf("Check the required key fields first. The worked records contain %d missing person key(s).", sum(is.na(record$person))),
          if (lvl == 1L) "Return the row identifier alongside is.na(person), retaining the input record even when its key is unusable."
          else "Tag roster rows with a presence flag before joining. Use na_matches = 'never' so missing keys cannot manufacture a match.",
          if (lvl > 1L) "Check first for an incomplete key, then an absent roster match, then a matched row with a missing value. The remaining rows have a recorded lookup value.",
          if (lvl == 3L) "Use both site and person in the match because a person can legitimately occur at another site."),
        if (lvl == 3L) "b = 1 if either site or person is missing, and b = 0 otherwise." else
          "b = 1 if person is missing, and b = 0 otherwise.",
        s("b", "Binary encoding of the row's logical incomplete-key check in is.na().",
          "Given the row and the required components of its join key."))
    },
    md_patterns = {
      fields <- input$fields[seq_len(if (lvl == 1L) 2L else 3L)]
      pattern <- paste(as.integer(!is.na(df[1L, fields, drop = FALSE])), collapse = "")
      missing_lesson("Field totals lose information about which gaps occur together. A response pattern keeps the column identity of every gap.",
        c(sprintf("For record %s, inspect %s in that order. Encoding observed as one and missing as zero yields '%s'.",
                  df$record_id[1L], paste(fields, collapse = ", "), pattern),
          "Unite the digits with an empty separator, then count rows for each observed string. Preserve leading zeroes by keeping the string as character.",
          if (lvl == 3L) "Count patterns within site and divide by that site's total count to get its share."),
        "Rᵢₖ = 1 for an observed entry and Rᵢₖ = 0 for a missing entry.",
        s("i", "Record index, tracked by record_id.", "Given the sampled rows of df.", "Index"),
        s("k", "Measurement index in the requested field order.", "Given the specified ordered field list.", "Index"),
        s("Rᵢₖ", "Observation indicator, as.integer(!is.na(value)).", "Given the displayed record and measurement.", "Observed realization"))
    },
    md_coabsence = {
      rows <- if (lvl == 3L) df$site == df$site[1L] else rep(TRUE, nrow(df))
      a <- is.na(df[[input$fields[1L]]][rows])
      b <- is.na(df[[input$fields[2L]]][rows])
      missing_lesson("Two marginal counts cannot tell you how many people missed both fields. Make the joint test on each record first.",
        c(sprintf("Build the missing flag for %s and the missing flag for %s. Within %s, %d records satisfy both.",
            input$fields[1L], input$fields[2L], if (lvl == 3L) df$site[1L] else "the displayed sample", sum(a & b)),
          if (lvl == 1L) "Sum the row-wise conjunction and return that count in a one-row data frame."
          else sprintf("The denominator for conditional_rate is the %d record(s) missing the first field. Return NA when that count is zero.", sum(a)),
          if (lvl == 3L) "For joint_rate use every site record in the denominator. Compute both rates anew within each site."),
        if (lvl == 1L) "d = number of records meeting both missing-cell conditions"
        else if (sum(a) > 0) sprintf("r = d / a = %d / %d ≈ %s", sum(a & b), sum(a), missing_number(sum(a & b) / sum(a)))
        else "a = 0, so r = d / a is undefined.",
        s("d", "Joint missing count, both.", "Given the measured pair and current group."),
        if (lvl > 1L) s("a", "Number missing the first field, first_absent.", "Same pair and group."),
        if (lvl > 1L) s("r", "Observed conditional fraction, conditional_rate.", "Among rows missing the first field, if any."))
    },
    md_followup = {
      visits <- input$visits
      measure <- setdiff(names(visits), c("id", "visit", "due"))[1L]
      observed <- visits$due & !is.na(visits[[measure]])
      id <- visits$id[which(observed)[1L]]
      last <- max(visits$visit[visits$id == id & observed])
      missing_lesson("A later measurement can establish that an earlier gap was intermittent. The end of the available record cannot establish why follow-up ended.",
        c(sprintf("Use scheduled rows with recorded %s to find each person's last observed visit. For person %s it is visit %d.", measure, id, last),
          "Join those last visits back to all scheduled rows. A person with no observed scheduled visit remains in the result.",
          if (lvl > 1L) "Label observed records first, then distinguish never-observed people, gaps before a later observation, and trailing gaps.",
          if (lvl == 3L) "Count intermittent and trailing gaps by person. Keep a separate never_observed flag so an entirely absent history stays identifiable."),
        sprintf("t_last = %d for person %s", last, id),
        s("t_last", "Last observed scheduled occasion, max(visit) giving last_observed.",
          "Given this person's due flags, visit labels and recorded measurements."))
    },
    md_rate_plot = {
      eligible <- df[df$due %in% TRUE, , drop = FALSE]
      r <- eligible[1L, , drop = FALSE]
      keep <- eligible$wave == r$wave
      if (lvl > 1L) keep <- keep & eligible$site == r$site
      if (lvl == 3L) keep <- keep & eligible$field == r$field
      n <- sum(keep)
      absent <- sum(is.na(eligible$value[keep]))
      missing_lesson("Each plotted point needs a clear opportunity count. Otherwise changes in who was due can look like changes in response.",
        c(sprintf("Filter to confirmed opportunities before grouping. The first example group at wave %d%s%s has %d missing among %d due.",
           r$wave, if (lvl > 1L) paste0(" in ", r$site) else "", if (lvl == 3L) paste0(" for ", r$field) else "", absent, n),
          "Compute mean(is.na(value)) in each group. Map wave and the fraction to point positions, fixing the vertical limits at zero and one.",
          if (lvl > 1L) "Map colour to site so site-specific rates remain identifiable.",
          if (lvl == 3L) "Wrap panels by field with shared axes. Each panel uses rates calculated for that field."),
        sprintf("r = d / e = %d / %d ≈ %s", absent, n, missing_number(absent / n)),
        s("d", "Missing eligible count contributing to the point.", "Given due records in the plotted group."),
        s("e", "Eligible record count contributing to the point.", "Given the wave and any site/field grouping.", "Observed count"),
        s("r", "Realized rate, the vertical aesthetic derived by mean(is.na(value)).", "Given the eligible group and its recorded statuses."))
    },
    md_overlap = {
      data <- input$data
      field <- setdiff(names(data), c("site", "risk", "person"))[1L]
      hill <- data$site == "Hill" & (lvl == 1L | data$risk == "higher")
      resp <- sum(!is.na(data[[field]][hill]))
      missing_lesson("A target group without a respondent supplies no direct measurement of its outcome. Grouping choices determine whether that gap is visible.",
        c(sprintf("Summarise eligible and recorded rows together. For %s, %d of %d values of %s are recorded.",
          if (lvl == 1L) "Hill" else "Hill's higher-risk group", resp, sum(hill), field),
          if (lvl == 1L) "Divide the recorded count by the eligible count for each site."
          else "Group by site and risk before testing for zero respondents. A zero sample count is evidence about observed support.",
          if (lvl == 3L) "Start from target when joining counts. Add target_count for both absent sample strata and present strata with no respondents, then divide by the entire target count."),
        if (lvl == 3L) "u = c / N" else sprintf("r = o / e = %d / %d ≈ %s", resp, sum(hill), missing_number(resp / sum(hill))),
        if (lvl == 3L) s("c", "Target people in unsupported strata, sum(unsupported).", "Given target counts and sample response flags.")
        else s("o", "Number of respondents, observed.", "Given the current site and requested risk grouping.", "Observed count"),
        if (lvl == 3L) s("N", "Size of the finite target population, sum(target_count).", "Given the target table.", "Fixed finite-population count")
        else s("e", "Number eligible in the group, eligible.", "Given the sample stratum.", "Observed count"),
        if (lvl == 3L) s("u", "Target share without sample respondent support, unsupported_share.", "Fixed target table and realized response counts.")
        else s("r", "Sample response fraction, response_rate.", "Given the eligible group and its observed responses."))
    },
    md_mechanism_rules = {
      rules <- input$rules
      pi <- rules$miss_prob[rules$protocol == rules$protocol[1L] &
        (lvl == 1L | rules$stratum == rules$stratum[1L])]
      missing_lesson("Here the full response law is supplied, including probabilities at scores that would go unmeasured. The comparison is possible because of that design.",
        c(sprintf("Inspect protocol %s%s. Its missing probabilities range from %s to %s.", rules$protocol[1L],
          if (lvl == 1L) " over the entire grid" else paste0(" within ", rules$stratum[1L]),
          missing_number(min(pi)), missing_number(max(pi))),
          if (lvl == 1L) "Count distinct probabilities within each protocol. A single distinct value means the probability is constant."
          else "Compare the two possible scores while holding stratum and protocol fixed. A change within any stratum reveals residual dependence on the potentially unobserved score.",
          if (lvl == 3L) "Assign MCAR to a constant law, MAR to variation explained entirely by the observed stratum, and MNAR to remaining score dependence. For this one-score missing pattern, miss_prob is the probability of an unrecorded score. Observed records alone leave that dependence unidentified."),
        sprintf("Δ = π_max − π_min = %s", missing_number(diff(range(pi)))),
        s("π_max", "Largest stipulated miss_prob among the cells being compared.", "Fixed protocol and selected grid cells.", "Fixed law summary"),
        s("π_min", "Smallest stipulated miss_prob among the same cells.", "Fixed protocol and selected grid cells.", "Fixed law summary"),
        s("Δ", "Probability range used by the distinct-value or min/max comparison.", "Fixed complete response law and chosen cells."))
    },
    md_equivalence = {
      cells <- input$cells
      a <- cells[cells$scenario == cells$scenario[1L], ]
      obs <- a$recorded
      successes <- sum(a$freq[obs] * a$outcome[obs])
      recorded <- sum(a$freq[obs])
      missing_lesson("Two complete-data explanations can generate exactly the same observed records. Hidden outcomes can change the full mean without changing any recorded value.",
        c("Use freq as the number of people represented by each cell. Decide which outcome values an analyst would actually see.",
          if (lvl == 1L) "Mask outcome in unrecorded cells, then sum frequencies for identical scenario, recorded and masked outcome states."
          else sprintf("In scenario %s, %d recorded successes among %d recorded people give the respondent mean. Divide the recorded count by all people for the response rate.", a$scenario[1L], successes, recorded),
          if (lvl == 3L) "Use all hypothetical cell frequencies for full_mean and subtract observed_mean. The resulting gap depends on the scenario's allocation of unrecorded outcomes."),
        sprintf("ȳ_obs = a / o = %d / %d ≈ %s", successes, recorded, missing_number(successes / recorded)),
        s("a", "Recorded successes, sum(freq × outcome) over recorded cells.", "Given the selected scenario's recorded cells."),
        s("o", "Recorded people, sum(freq) over recorded cells.", "Same scenario and response flag.", "Supplied frequency total"),
        s("ȳ_obs", "Observed respondent mean, observed_mean in the summary levels.", "Fixed recorded outcomes and their frequencies."))
    },
    NULL)
}

guided_missing_resolution <- function(ex) {
  lvl <- ex$level
  input <- ex$inputs
  df <- input$df
  s <- missing_symbol
  switch(ex$id,
    missing_source_reconcile = {
      valid <- is.na(df$signed) & !is.na(df$field) & (lvl < 3L | !df$disputed)
      row <- which(valid)[1L]
      missing_lesson("A second registry can recover a value when a documented priority rule authorizes the source.",
        c(sprintf("For record %s the signed value is missing. The eligible field count is %s, so the priority rule supplies that count.", df$id[row], df$field[row]),
          if (lvl == 1L) "Use coalesce() in authority order. Two missing reports leave the result missing."
          else "Keep the value and record the source that supplied it. If neither source is eligible, label the source unknown.",
          if (lvl == 3L) "Exclude disputed field values before fallback. Mark disagreements and disputed nonmissing field reports for review, even when a signed value is available."),
        sprintf("h(record %s) = %s", df$id[row], df$field[row]),
        s("h", "Source-priority rule producing resolved via coalesce().", "Given the recorded reports, priority policy and applicable dispute rule.",
          "Deterministic function", "One registry record → one numeric count or missing"))
    },
    missing_identity = {
      rows <- which(df$applicable & is.na(df$kept) & !is.na(df$shipped) & !is.na(df$returned) & df$shipped > df$returned)
      i <- rows[1L]
      a <- df$shipped[i]
      b <- df$returned[i]
      missing_lesson("An accounting identity can identify a missing component exactly when the inputs and the identity both apply.",
        c(sprintf("For shipment %s, %d shipped and %d returned leave %d kept. This row is applicable and kept was missing.", df$id[i], a, b, a - b),
          "Preserve a supplied kept count. Reconstruct only a missing count from a known, nonnegative remainder, and leave structural rows empty.",
          if (lvl > 1L) "Compare a computable remainder with the recorded count and label contradictions before assigning recorded or reconstructed status.",
          if (lvl == 3L) "For kept_share require an admissible record, a positive known shipped count, and a resolved count at most as large as shipped."),
        sprintf("c = a − b = %d − %d = %d", a, b, a - b),
        s("a", "Recorded shipment total, shipped.", "Given the applicable shipment.", "Observed count"),
        s("b", "Recorded return count, returned.", "Same shipment and accounting period.", "Observed count"),
        s("c", "Reconstructed retained count, resolved.", "Given both counts and the stipulated accounting identity."))
    },
    missing_carry = {
      missing_lesson("A device setting is persistent under the stated equipment rule. Carrying an older setting requires evidence that the rule is still valid at the new hour.",
        c("Arrange by device, shift and hour. A leading gap stays empty because there is no earlier reading within that timeline.",
          if (lvl == 1L) "Fill downward within device and shift only. Start a fresh history at each new shift."
          else "Carry both the last observed setting and its actual reading hour. Repeated empty rows do not refresh that clock.",
          if (lvl == 2L) "For each gap, compare elapsed time with the stated expiry window and clear expired values.",
          if (lvl == 3L) "Start a new run at each reset, then enforce expiry within that run. The reset row can establish a new observed setting."),
        "d = t − s",
        s("t", "Current occasion, hour on the row being resolved.", "Given one device, shift and ordered timeline.", "Observed occasion"),
        s("s", "Most recent eligible measured hour, last_read in the expiry levels.", "Same device, shift and applicable reset history.", "Observed occasion selected by the rule"),
        s("d", "Age of the carried reading in hours, hour − last_read.", "Given both occasions and the persistence rule."))
    },
    missing_eligible = {
      rows <- df$eligible
      if (lvl > 1L) rows <- rows & df$site == df$site[which(rows)[1L]]
      cc <- stats::complete.cases(df[rows, c("score", "exposure", "baseline")])
      total <- length(cc)
      retained <- sum(cc)
      missing_lesson("Complete-case status belongs to an analysis. A missing optional note does not remove the measurements required by this model.",
        c("Restrict to eligible records before checking the three required fields with if_all().",
          sprintf("In %s, %d of %d eligible records satisfy all three checks. The remaining %d are excluded.",
            if (lvl == 1L) "the cohort" else paste("site", df$site[which(rows)[1L]]), retained, total, total - retained),
          if (lvl >= 2L) "Report counts and the retained share within each represented site.",
          if (lvl == 3L) "Use complete rows for complete_mean and all eligible recorded scores for available_mean. Return a missing mean when its corresponding subset is empty."),
        sprintf("d = e − c = %d − %d = %d", total, retained, total - retained),
        s("e", "Eligible count, eligible_n.", "Given eligibility and the requested site grouping.", "Observed count"),
        s("c", "Eligible records complete on the three required fields, complete_n.", "Same group and fixed model requirements."),
        s("d", "Excluded eligible records, excluded_n.", "Same eligible cohort and complete-case rule."))
    },
    missing_observation_weight = {
      i <- which(!is.na(df$score))[1L]
      prob <- df$p_observed[i]
      missing_lesson("A respondent with a lower known observation probability represents more people in the weighted summary.",
        c(sprintf("For the displayed respondent in site %s, stratum %s, the observation probability is %s. Its inverse weight is %s.",
          df$site[i], df$stratum[i], missing_number(prob), missing_number(1 / prob)),
          "Filter to recorded scores. Sum score times weight and divide by the sum of those same respondent weights. This represents the eligible cohort under the exercise's positive response probabilities and random selection within site and stratum.",
          if (lvl >= 2L) "Repeat within each site and compare the result with its unweighted respondent mean.",
          if (lvl == 3L) "Square the sum of weights and divide by the sum of their squares for effective_n. Concentrated weights reduce that effective count."),
        sprintf("w = 1 / π = 1 / %s ≈ %s", missing_number(prob), missing_number(1 / prob)),
        s("π", "Observation probability, p_observed.", "Conditional on the recorded site and stratum under the stipulated response law.", "Known fixed probability"),
        s("w", "Inverse probability weight, weight.", "Given the positive observation probability and the stated conditional response assumption."))
    },
    missing_delta_grid = {
      shift <- if (lvl == 1L) input$shift else min(input$scenarios$shift)
      i <- which(is.na(df$score))[1L]
      predicted <- df$prediction[i]
      missing_lesson("Sensitivity analysis changes an assumption about the unobserved outcomes and measures how much the target mean moves.",
        c(sprintf("Consider a missing score with baseline prediction %s. Under shift %s its scenario mean is %s.",
                  predicted, shift, predicted + shift),
          "Use coalesce() to preserve every measured score and apply the shifted prediction only to gaps. Average across the whole cohort.",
          if (lvl > 1L) "Cross each record with the supplied scenarios. Calculate a separate cohort mean for each shift before testing the strict threshold.",
          if (lvl == 3L) "Of the tested shifts that cross the threshold, take the one closest to zero. Report NA when none crosses."),
        sprintf("μ_shift = μ_base + δ_sens = %s + (%s) = %s", predicted, shift, predicted + shift),
        s("μ_base", "Baseline working conditional mean for this missing outcome, prediction.", "Given the supplied prediction for this person.", "Fixed working input"),
        s("δ_sens", "Sensitivity shift in score units, shift.", "Chosen scenario for the missing outcomes.", "Fixed sensitivity parameter"),
        s("μ_shift", "Scenario conditional mean used for the missing score, prediction + shift.", "Given the baseline working model and chosen shift.", "Deterministic scenario value"))
    },
    missing_binary_bounds = {
      groups <- if (lvl == 1L) rep("all", nrow(df)) else df$cohort
      bounds <- lapply(split(df$event, groups), function(y) {
        c(lower = sum(y == 1L, na.rm = TRUE) / length(y),
          upper = (sum(y == 1L, na.rm = TRUE) + sum(is.na(y))) / length(y))
      })
      label <- names(bounds)[1L]
      bound <- bounds[[1L]]
      if (lvl == 3L) {
        return(missing_lesson("To bound a difference, choose each cohort's missing outcomes in the direction that makes the contrast most extreme.",
          c("For each cohort, divide the observed event count by all enrolled units for its lower endpoint. Add all missing events to the numerator for its upper endpoint.",
            "For the smallest B-minus-A difference, combine the lower endpoint in B with the upper endpoint in A. Reverse those choices for the largest difference.",
            "These extreme assignments can occur jointly because the two cohorts have separate missing outcomes."),
          sprintf("L_θ = l_B − u_A ≈ %s     U_θ = u_B − l_A ≈ %s",
            missing_number(bounds$B["lower"] - bounds$A["upper"]), missing_number(bounds$B["upper"] - bounds$A["lower"])),
          s("θ", "Target contrast in realized finite-cohort event fractions, B minus A.", "Fixed enrolled cohorts and binary outcomes, including unrevealed values.", "Fixed, partially identified target"),
          s("A, B", "The two labels in the cohort column.", "Given the study's partition into two disjoint cohorts.", "Fixed labels", "Two categorical labels"),
          s("l_A", "Smallest event fraction in cohort A, lower for A.", "A's observed events, with all missing outcomes set to zero."),
          s("u_A", "Largest event fraction in cohort A, upper for A.", "A's observed events, with all missing outcomes set to one."),
          s("l_B", "Smallest event fraction in cohort B, lower for B.", "B's observed events, with all missing outcomes set to zero."),
          s("u_B", "Largest event fraction in cohort B, upper for B.", "B's observed events, with all missing outcomes set to one."),
          s("L_θ", "Sharp lower endpoint for the B-minus-A contrast, lower_diff.", "Given both cohorts' observed records and unrestricted missing binary outcomes."),
          s("U_θ", "Sharp upper endpoint for that contrast, upper_diff.", "Given the same records and unrestricted missing binary outcomes.")))
      }
      missing_lesson("An unobserved binary event can contribute either zero or one. Retaining everyone in the denominator gives attainable extremes.",
        c("Assign every unknown outcome zero for the lower endpoint. Count the observed ones and divide by the full group size.",
          sprintf("Assign every unknown outcome one for the upper endpoint. For %s the two fractions are %s and %s.",
            if (lvl == 1L) "the entire cohort" else paste("cohort", label), missing_number(bound[1L]), missing_number(bound[2L]))),
        "L_θ = a / n     U_θ = (a + d) / n",
        s("θ", "Realized event fraction among the enrolled units in the target group.", "Fixed cohort and its binary outcomes, partly unobserved.", "Fixed, partially identified target"),
        s("a", "Number of observed events, sum(event == 1, na.rm = TRUE).", "Given recorded events in this group.", "Observed count"),
        s("d", "Number with missing event status, sum(is.na(event)).", "Given this cohort's response flags.", "Observed count"),
        s("n", "Number of sampled enrolled units in this group, n().", "Fixed cohort membership.", "Observed count"),
        s("L_θ", "Sharp lower endpoint, lower.", "Given cohort membership, observed events, and the feasible all-zero assignment to missing outcomes."),
        s("U_θ", "Sharp upper endpoint, upper.", "Given the same observed records and the feasible all-one assignment to missing outcomes."))
    },
    missing_compare_reports = {
      point <- stats::setNames(input$point$estimate, input$point$method)
      a <- unname(point["respondents"])
      b <- unname(point["weighted"])
      missing_lesson("The original estimate answers the comparison question. The resamples show its variability under the stated within-stratum resampling design.",
        c(sprintf("Align the original method labels before subtracting. The weighted mean is %s and the respondent mean is %s.", missing_number(b), missing_number(a)),
          if (lvl == 1L) "Return both original values and their signed difference, weighted minus respondents."
          else if (lvl == 2L) "Take the default 2.5th and 97.5th percentiles separately within each method's bootstrap estimates. Join them to the original estimate."
          else "Pivot by shared bootstrap draw first. Subtract the two methods within each draw, then take percentiles of those paired differences.",
          if (lvl == 3L) "Check whether the endpoints straddle or equal zero. Interpret the interval with the respondent counts and observation probabilities held fixed."),
        sprintf("ȳ_w ≈ %s     ȳ_obs ≈ %s     Δ̂ = ȳ_w − ȳ_obs ≈ %s", missing_number(b), missing_number(a), missing_number(b - a)),
        s("ȳ_w", "Weighted original-sample mean, point estimate for weighted.", "Fixed original respondents and observation probabilities.", "Realized estimate"),
        s("ȳ_obs", "Unweighted respondent mean, point estimate for respondents.", "Fixed original respondents and recorded scores.", "Realized estimate"),
        s("Δ̂", "Original-sample change, weighted − respondents.", "Given the paired original estimates.", "Realized estimate"))
    },
    missing_train_only = {
      train <- input$train
      median_length <- stats::median(train$length_cm, na.rm = TRUE)
      missing_lesson("A held-out evaluation should reproduce deployment. The replacement rule must already be learned when a new record arrives.",
        c(sprintf("Compute summaries from recorded training measurements. The global training median of length_cm here is %s.", missing_number(median_length)),
          if (lvl == 1L) "Apply the frozen median to missing held-out lengths using coalesce(). Copy every recorded length unchanged."
          else "Learn site medians on training measurements only. Join them to the held-out rows and fall back to the feature's global training median when a site has no training value.",
          if (lvl == 3L) "Work in long form by feature. Preserve the original was_missing flag and distinguish observed, site and global sources."),
        sprintf("q₀.₅ ≈ %s cm for the training lengths", missing_number(median_length)),
        s("q₀.₅", "Empirical median of observed training length_cm, the global fallback.", "Conditional on observed training lengths for this feature.", "Realized sample quantile"))
    },
    missing_mask_validation = {
      data <- input$evaluation
      data <- data[data$masked & !is.na(data$truth), ]
      valid <- !is.na(data$prediction)
      if (lvl > 1L) valid <- valid & !is.na(data$lower) & !is.na(data$upper)
      if (lvl == 3L) {
        pairs <- tapply(valid, data$id, function(z) length(z) == 2L && all(z))
        valid <- data$id %in% as.integer(names(pairs)[pairs])
      }
      row <- data[which(valid & data$method == "local")[1L], , drop = FALSE]
      missing_lesson("An intentionally masked measurement has a known truth for scoring. A genuine gap cannot tell you how close a prediction was.",
        c(sprintf("Restrict to masked records with truth. For local prediction on ID %s, prediction %s minus truth %s gives signed error %s.",
                  row$id, missing_number(row$prediction), missing_number(row$truth), missing_number(row$prediction - row$truth)),
          if (lvl == 1L) "Keep local predictions that exist. Take the absolute value of each error before averaging it for mae."
          else "Use cases with a prediction and both interval endpoints. Summarise absolute and signed errors separately, and count truths at either endpoint as covered.",
          if (lvl == 3L) "Count eligible IDs by site before exclusions. Compare both methods on identical complete pairs, including checks for an entirely absent method row."),
        sprintf("ŷ ≈ %s     y ≈ %s     e = ŷ − y ≈ %s", missing_number(row$prediction), missing_number(row$truth), missing_number(row$prediction - row$truth)),
        s("ŷ", "Held-out point prediction for this deliberately masked measurement, prediction.", "Fixed method, training data and allowed predictors.", "Realized prediction"),
        s("y", "Previously observed measurement held out for scoring, truth.", "Given the selected masked evaluation case.", "Observed realization"),
        s("e", "Signed prediction error, prediction − truth, used for signed_error and mae.", "Given the paired prediction and held-out truth."))
    },
    NULL)
}

guided_missing_imputation <- function(ex) {
  lvl <- ex$level
  input <- ex$inputs
  df <- input$df
  imp <- input$imp
  if (ex$id %in% c("mice_complete", "mice_chains", "mice_distributions", "mice_pool")) {
    target <- names(imp$method)[imp$method != ""][1L]
  }
  s <- missing_symbol
  switch(ex$id,
    mice_methods = {
      numeric_target <- setdiff(names(df), c("id", "enrolled", "channel", "rating", "archived"))[1L]
      method <- if (lvl == 3L) "norm" else "pmm"
      missing_lesson("Choose the response model for each incomplete field from its measurement type and the study's imputation plan.",
        c(sprintf("Inspect %s and enrolled. The first is numeric with gaps. The second is an incomplete two-level factor, so its default method is logreg.", numeric_target),
          if (lvl == 1L) sprintf("Select %s followed by enrolled. make.method() produces method names in precisely that order.", numeric_target)
          else "Retain every column in its original order and leave complete fields with an empty method. Distinguish the unordered channel factor from the ordered rating factor.",
          if (lvl == 3L) "Supply the requested normal linear model for incomplete numeric measurements, then override archived with an empty method. Its gaps are outside the analysis. For a derived field in a later study, recompute its defining relationship from the completed inputs or specify a passive formula."),
        sprintf("h(%s) = '%s'    h(enrolled) = 'logreg'", numeric_target, method),
        s("h", "Method selection for a named field in mice::make.method().",
          "Fixed field class, missingness pattern, factor levels and exercise-specific policy.",
          "Deterministic function", "Field metadata and policy → one method string (possibly empty)"))
    },
    mice_predictors = {
      id <- grep("_id$", names(df), value = TRUE)[1L]
      target <- names(df)[vapply(df, anyNA, logical(1))][1L]
      dims <- sprintf("Matrix, %d × %d", ncol(df), ncol(df))
      missing_lesson("Read the matrix across a target row to see which variables can inform its imputation. An identifier can label a person without being a meaningful numeric predictor.",
        c(sprintf("Keep both sets of dimension names. The entry in row %s and column %s must be zero in this example.", target, id),
          if (lvl == 1L) sprintf("Set the %s predictor column to zero across all rows and leave the rest of the default matrix intact.", id)
          else if (lvl == 2L) "Start with zeroes and enable baseline and auxiliary for the incomplete target only. At prediction time post_visit is unavailable."
          else "In this retrospective study the analysis outcome is recorded. Enable it, baseline, auxiliary and baseline_aux for the incomplete exposure. The product preserves the stipulated covariate interaction in its conditional model.",
          "Restore the target row names before converting the table back to the numeric matrix expected by mice."),
        "𝖯ₖℓ = 1 permits the predictor and 𝖯ₖℓ = 0 excludes it.",
        s("𝖯", "Imputation predictor matrix, predictorMatrix in mice().", "Given the selected targets, available predictors and analysis purpose.",
          "Fixed model specification", dims),
        s("k", "Target-variable row index in the matrix.", "Given the df column order used as target order.", "Index"),
        s("ℓ", "Candidate-predictor column index.", "Given the df column order.", "Index"),
        s("𝖯ₖℓ", "Binary entry in row k and column ℓ of predictorMatrix.", "Fixed predictor-selection policy for that target.", "Fixed model specification"))
    },
    mice_where = {
      target <- setdiff(names(df), c("eligible", "baseline", "check_observed"))[1L]
      structural <- sum(!df$eligible & is.na(df[[target]]))
      missing_lesson("The where mask tells the imputer which individual cells should receive a draw.",
        c(sprintf("Create make.where(df). Its %d rows refer to people and its %d columns follow the input variables.", nrow(df), ncol(df)),
          if (lvl == 1L) "Return the default logical mask. At this level every NA is marked for a draw."
          else sprintf("For %s, clear all %d inapplicable missing cells. Keep eligible gaps selected and preserve the default rule in the other columns.", target, structural),
          if (lvl == 3L) sprintf("Additionally select the %d eligible measured cells flagged for validation. Overimputation lets you compare a generated value with the original observation.", sum(df$eligible & df$check_observed))),
        if (lvl == 1L) "𝖶ᵢₖ = TRUE exactly where df has a missing cell." else
          if (lvl == 2L) "For the designated measurement, 𝖶ᵢₖ = TRUE exactly for eligible missing cells." else
            "For the designated measurement, 𝖶ᵢₖ = TRUE for eligible gaps and eligible validation cells.",
        s("𝖶", "The logical cell mask, where in mice().", "Given the displayed data and cell-selection policy.", "Fixed imputation specification",
          sprintf("Matrix, %d × %d", nrow(df), ncol(df))),
        s("i", "Person row index in df.", "Given this data frame's row order.", "Index"),
        s("k", "Variable column index in df.", "Given its column order.", "Index"),
        s("𝖶ᵢₖ", "Logical decision for the selected cell in where.", "Given its eligibility and missingness, plus any explicit overimputation flags.", "Fixed cell-selection decision"))
    },
    mice_fit = {
      target <- names(df)[vapply(df, anyNA, logical(1))][1L]
      missing_lesson("One completed data set is one version of the unknown cells. Iterations update each chain before you extract those versions.",
        c(sprintf("Supply the fixed seed %d and choose predictive mean matching for %s. Here draws is %d and cycles is %d.", input$fit_seed, target, input$draws, input$cycles),
          if (lvl == 1L) "Run with the default predictor matrix, then extract completion one, retaining the input row order."
          else "Pass the supplied predictors matrix. Its active target row uses baseline and auxiliary, which are available at prediction time.",
          if (lvl == 3L) "Pass where as well. The mask protects inapplicable gaps even though they contain NA.",
          if (lvl > 1L) "Extract long output with .imp and .id. A positive .imp identifies a completion. Omit the original incomplete records.",
          "Increasing maxit gives each chain more time to settle. Increasing m reduces simulation noise in pooled estimates. If loggedEvents records a dropped predictor, inspect its variation and its relationship to the other predictors."),
        sprintf("u = M × h = %d × %d = %d", input$draws, input$cycles, input$draws * input$cycles),
        s("M", "Number of imputed data sets, draws passed as m.", "Given the exercise settings.", "Fixed count"),
        s("h", "Iterations per chain, cycles passed as maxit.", "Given the exercise settings.", "Fixed count"),
        s("u", "Scheduled target updates across chains after initialization.", "One active target, with the specified number of chains and iterations."))
    },
    mice_complete = {
      original <- imp$data
      missing_id <- original$unit[which(is.na(original[[target]]))[1L]]
      missing_lesson("The same person appears in every completed data set. Keep their identifier and the completion identifier separate.",
        c(sprintf("The fitted imp object stores %d people and %d completions. Unit %s had no original %s.", nrow(original), imp$m, missing_id, target),
          if (lvl == 1L) "Give the requested completion number to mice::complete(). Check the original row order and retain all columns."
          else if (lvl == 2L) "Request long output with include = TRUE. Index zero holds the original incomplete rows and positive indices hold the completed rows. Select the three requested columns."
          else "Identify the originally missing units from imp$data before extracting the positive completions. Group by unit across those completions and take the smallest and largest generated value."),
        sprintf("𝒟⁽ᵐ⁾ has %d rows for each m from 1 through M = %d.", nrow(original), imp$m),
        s("M", "Number of completed data sets, imp$m.", "Given this fitted imputation object.", "Fixed imputation setting"),
        s("m", "Completion index, a positive action number or .imp value.", "Given the stored completions.", "Index"),
        s("𝒟⁽ᵐ⁾", "One completed data set returned by mice::complete(). The superscript identifies an imputation.",
          "Given the original records and this stored imputation draw.", "Realized completed data set",
          sprintf("Data frame, %d × %d", nrow(original), ncol(original))))
    },
    mice_chains = {
      means <- imp$chainMean[target, , , drop = TRUE]
      chain <- colnames(means)[1L]
      last <- nrow(means)
      if (lvl == 2L) {
        sd <- sqrt(imp$chainVar[target, last, ])
        return(missing_lesson("Imputed values have a spread within each chain. Comparing that spread across chains can reveal chains exploring different regions.",
          c(sprintf("Extract %s from the variable dimension of chainVar. At iteration %d, take the square root of each of the %d stored variances.", target, last, length(sd)),
            sprintf("Within that iteration the smallest and largest chain standard deviations are %s and %s. Repeat for every iteration after reshaping the chain columns.", missing_number(min(sd)), missing_number(max(sd))),
            "Investigate persistent separation or drift. These deliberately short practice chains cannot certify convergence."),
          "s = √v",
          s("v", "Stored sample variance of imputed values in one chain, imp$chainVar.", "Fixed target, iteration and realized chain.", "Realized diagnostic"),
          s("s", "Sample standard deviation used to compute low and high.", "Same target, iteration and chain.")))
      }
      missing_lesson("Each trace follows the mean of imputed values through the updates of one chain. Mixing and persistent drift are easier to see when the chains retain their identities.",
        c(sprintf("Slice imp$chainMean for %s. In %s the final recorded mean is %s at iteration %d.", target, chain, missing_number(means[last, 1L]), last),
          if (lvl == 1L) "Select the final iteration for every chain. Convert the named vector into a table with the original chain name and mean."
          else "Stack the chain columns, turn iteration labels into numbers and order the rows within each chain. Subtract the preceding mean within each chain. The first change is undefined."),
        if (lvl == 1L) sprintf("a = %s in %s at iteration %d", missing_number(means[last, 1L]), chain, last)
        else sprintf("a ≈ %s     b ≈ %s     d = a − b ≈ %s", missing_number(means[last, 1L]), missing_number(means[last - 1L, 1L]),
                     missing_number(means[last, 1L] - means[last - 1L, 1L])),
        s("a", "Mean of imputed values for a selected chain and iteration, chain_mean from imp$chainMean.", "Fixed target, chain and realized iteration.", "Realized diagnostic"),
        if (lvl == 3L) s("b", "Mean at the preceding iteration in this chain, lag(chain_mean).", "Same target and chain, one iteration earlier.", "Realized diagnostic"),
        if (lvl == 3L) s("d", "Within-chain change in the mean, change.", "Given the two successive means in the same chain."))
    },
    mice_distributions = {
      original <- imp$data
      rows <- if (lvl == 3L) original$stratum == original$stratum[1L] else rep(TRUE, nrow(original))
      measured <- original[[target]][rows & !is.na(original[[target]])]
      missing_lesson("An imputation fills the cell and erases its visible NA. Recover the source label from the original records before comparing distributions.",
        c(sprintf("Use unit to identify which %s values were recorded originally. In %s, %d were measured and %d will be imputed per completion.",
            target, if (lvl == 3L) paste("stratum", original$stratum[1L]) else "the displayed cohort", length(measured), sum(rows) - length(measured)),
          if (lvl == 1L) "Extract the requested completion and label each value using that original status. Calculate counts and means separately for the two sources."
          else "Extract positive completions in long form and retain .imp when summarising. Calculate count, mean and sample standard deviation by source in each completion.",
          if (lvl == 3L) "Also group by stratum. Compare like groups before deciding whether location or spread looks implausible. Missingness can select people with different observed predictors, so differences in marginal averages can be compatible with MAR."),
        sprintf("a ≈ %s     o = %d     ȳ_obs = a / o ≈ %s", missing_number(sum(measured)), length(measured), missing_number(mean(measured))),
        s("a", "Sum of originally measured target values contributing to average.", "Fixed original source group and requested stratum."),
        s("o", "Number originally observed, records for the observed group.", "Same original group.", "Observed count"),
        s("ȳ_obs", "Observed-group mean in each selected completion, average.", "Given the originally recorded values in that group.", "Realized sample mean"))
    },
    mice_pool = {
      settings <- sprintf("imp contains %d completed data sets. Use with(imp, lm(...)) to fit the same response and predictors in each one.", imp$m)
      scope <- "Same coefficient across the realized completed-data fits."
      count <- s("M", "Number of completed-data models pooled, imp$m.", "Given the fitted imputation object.", "Fixed imputation count")
      label <- s("MI", "Subscript labelling a multiple-imputation summary.", "Not applicable to a label.",
                 "Fixed descriptive label", "Not applicable to a descriptive label.")
      if (lvl == 1L) {
        return(missing_lesson("Fit first and combine estimates second. Stacking repeated versions of a person as independent observations would understate uncertainty.",
          c(settings, "Pool the fitted models with mice::pool(). Match coefficients by term and report their pooled estimates and standard errors, including the intercept.",
            "The point estimate is the mean of that term's completed-data estimates. Its standard error includes both variance within fitted models and disagreement between their estimates."),
          "θ̂_MI = (1 / M) ∑ₘ θ̂⁽ᵐ⁾",
          count, s("m", "Index of one completion in with(imp, lm(...)).", "Given the fixed list of M fits.", "Index"),
          s("θ", "Population regression coefficient for the selected lm() term.", "Under the specified frequentist linear model and its predictors.", "Fixed, unknown parameter"),
          s("θ̂⁽ᵐ⁾", "Point estimate of that coefficient from the m-th completed-data fit.", "Given the realized completion and fitted model.", "Realized estimate"),
          s("θ̂_MI", "Pooled point estimate, estimate in summary(pooled).", scope, "Realized estimate"), label))
      }
      missing_lesson("Imputation uncertainty has two parts. The fitted models have sampling variance, and their coefficient estimates vary across completions.",
        c(settings, sprintf("Adjust for baseline in every fit before pooling. Use the same %s coefficient across completions when calculating its between-imputation variance.", target),
          if (lvl == 2L) "Request the summary with confidence limits. Use the adjusted degrees of freedom returned by mice for the two-sided 95% interval on each coefficient."
          else "For the requested term, extract ubar, b and t. Return sqrt(t) as the standard error. The variance share lambda and the degrees-of-freedom-adjusted fmi are distinct quantities."),
        if (lvl == 2L) "T_MI = Ū_MI + (1 + 1 / M) B_MI     SÊ_MI = √T_MI"
        else "T_MI = Ū_MI + (1 + 1 / M) B_MI     λ_MI = (1 + 1 / M) B_MI / T_MI",
        count, s("Ū_MI", "Mean estimated within-imputation variance for the coefficient, ubar.", scope),
        s("B_MI", "Sample variance across the completed-data coefficient estimates, b.", scope),
        s("T_MI", "Total estimated coefficient variance in pooled$pooled$t.", scope),
        if (lvl == 2L) s("SÊ_MI", "Estimated pooled standard error, std.error.", "Given the total estimated variance for that term.", "Realized uncertainty estimate")
        else s("λ_MI", "Proportion of total variance attributable to missingness, lambda.", "Given the positive total variance, between-imputation variance and number of fits."),
        label)
    },
    NULL)
}
