# Each notation table belongs to one worked example. Literal dimensions refer
# to that example, including any row subset named in the walkthrough.
core_scalar <- function(symbol, meaning, status, given) {
  c(symbol, meaning, "1 × 1 scalar", status, given)
}

core_fn <- function(symbol, meaning, map, given) {
  c(symbol, meaning, map, "Deterministic function", given)
}

core_fixed <- function(symbol, meaning) {
  core_scalar(symbol, meaning, "Fixed task input", "Specified by the worked prompt.")
}

core_input <- function(symbol, meaning) {
  core_scalar(symbol, meaning, "Observed value", "The displayed example and selected row are held fixed.")
}

core_output <- function(symbol, meaning, given = "The worked inputs and task rule are held fixed.") {
  core_scalar(symbol, meaning, "Deterministic result", given)
}

core_num <- function(x) format(x, digits = 5, trim = TRUE)

core_nrow <- function(df) {
  core_scalar("n", sprintf("Number of input records, nrow(df) = %d.", nrow(df)),
              "Observed count", "The input table is held fixed.")
}

core_group_rows <- function(df, ctx, level, row = 1L) {
  keep <- df[[ctx$g]] == df[[ctx$g]][row]
  if (level >= 2L) keep <- keep & df[[ctx$h]] == df[[ctx$h]][row]
  which(keep)
}

core_group_name <- function(df, ctx, level, row = 1L) {
  keys <- c(ctx$g, if (level >= 2L) ctx$h)
  paste(vapply(keys, function(k) sprintf("%s = %s", k, as.character(df[[k]][row])), ""), collapse = ", ")
}

core_summary_name <- function(ex) {
  candidates <- c("median", "mean", "sum")
  present <- vapply(candidates, function(name) grepl(paste0(name, "("), ex$solution, fixed = TRUE), logical(1))
  candidates[which(present)[1L]]
}

core_basics_notes <- function(ex, ctx) {
  level <- ex$level
  values <- ex$inputs$values
  if (ex$id == "vector_assignment") {
    raw <- values[1]
    value <- raw * ctx$by - if (level >= 2L) ctx$cut else 0
    if (level == 3L) value <- round(value)
    arithmetic <- if (level == 1L) "a × v" else "a × v − c"
    symbols <- list(core_input("v", "One element of values."), core_fixed("a", sprintf("Multiplier %d in the first expression.", ctx$by)))
    if (level >= 2L) symbols <- c(symbols, list(core_fixed("c", sprintf("Offset %d subtracted after multiplication.", ctx$cut))))
    if (level == 3L) {
      arithmetic <- paste0("round(", arithmetic, ")")
      symbols <- c(symbols, list(core_fn("round", "R's round() to the nearest whole number, with ties to even.",
                                        "Numeric scalar → numeric scalar", "Zero decimal places.")))
    }
    return(guided_notes("One vector expression applies the same arithmetic independently at every position.",
      c(sprintf("Trace the first value, %s. Multiplying by %d gives %s.", core_num(raw), ctx$by, core_num(raw * ctx$by)),
        switch(level, "Assign the result for the whole vector with <-.",
               sprintf("Subtract %d after the multiplication and assign the whole vector.", ctx$cut),
               sprintf("Subtract %d inside parentheses, then pipe the result into round().", ctx$cut)),
        "Finish with adjusted so the grader receives every transformed element in input order."),
      sprintf("u = %s = %s   (first position)", arithmetic, core_num(value)),
      c(symbols, list(core_output("u", "The corresponding element of adjusted.")))))
  }
  if (ex$id == "vector_index") {
    cutoff <- ctx$by
    op <- sub(".*satisfying \\(?values (>=|>|<) .*", "\\1", ex$instructions)
    direction <- if (op == "<") -1L else 1L
    exclude <- cutoff + direction
    condition <- paste("v", if (op == ">=") "≥" else op, cutoff)
    if (level >= 2L) condition <- sprintf("%s and v ≠ %d", condition, exclude)
    return(guided_notes("A logical index answers one yes-or-no question at each position. Extracting the true positions preserves their encounter order.",
      c(sprintf("Compare each value with %d using the prompt's %s boundary. A value exactly on the boundary distinguishes a strict comparison from an inclusive one.", cutoff, op),
        if (level >= 2L) sprintf("Use & to exclude %d as well. Both conditions must hold for the same element.", exclude)
        else "Put the resulting logical vector inside values[...]. A repeated qualifying value supplies two output positions.",
        if (level == 3L) "Apply unique() after filtering. Its first-occurrence order is the order requested."),
      sprintf("b = 𝟙{%s}   (before any duplicate removal)", condition),
      list(core_input("v", "One numeric element of values."), core_output("b", "Selection flag, TRUE/FALSE in the bracket expression, represented as 1/0 here."),
           core_fn("𝟙{·}", "Indicator of the comparison, corresponding to the logical test inside values[...].",
                   "Event truth value → {0, 1}", "The prompt's cutoffs and current element are fixed."))))
  }
  if (ex$id == "conditional_function") {
    labels <- if (level == 1L) "unknown, low, high" else "unknown, low, middle, high"
    return(guided_notes("A scalar classification function chooses one label per call. Order the branches so an exceptional input can exit before any numeric comparison.",
      c(if (level == 3L) "Handle NA and both infinities with !is.finite(value) first."
        else "Handle is.na(value) first. Otherwise if() may receive an indeterminate comparison.",
        sprintf("Use a strict lower comparison at %d. An input exactly equal to %d belongs to %s.", ctx$cut, ctx$cut, if (level == 1L) "high" else "middle"),
        if (level >= 2L) sprintf("Test the second boundary, %d, next. Reaching the final branch returns high.", ctx$cut + ctx$by),
        "Return the function object itself. The checker calls it on several fresh arguments."),
      sprintf('h(%d) = "%s"', ctx$cut, if (level == 1L) "high" else "middle"),
      list(core_fn("h", "The submitted function(value). The argument here equals the lower boundary.",
                   paste0("One numeric value, including NA or infinity → one of ", labels), "The branch order and cutoffs in this worked rule are fixed."))))
  }
  sum_path <- grepl("total + value", ex$solution, fixed = TRUE)
  start <- if (sum_path) "0" else "−∞"
  rule <- if (sum_path) "u + v" else "max(u, v)"
  symbols <- list(core_output("u", "Current accumulator, total before this iteration.", "Previous elements, the start value and the missing-value rule are fixed."),
                  core_input("v", "Current finite element, value inside the for loop."),
                  core_output("u′", "Updated total copied into the next answer slot.", "The previous total and the current element are fixed."))
  if (!sum_path) symbols <- c(symbols, list(core_fn("max", "max(total, value) chooses the larger argument.",
                    "Pair of extended-real scalars → one extended-real scalar", "This is the running-maximum rule.")))
  second <- if (level == 1L) {
    if (sum_path) values[1] + values[2] else max(values[1:2])
  } else if (level == 2L) values[1] else if (sum_path) 0 else -Inf
  guided_notes("The accumulator is a one-number memory of the prefix already processed. Save its updated value at each position to keep the whole path.",
    c(sprintf("Allocate answer with length(values) slots and initialize total to %s. This makes the first observed element work even if it is negative.", start),
      sprintf("For each finite element, %s and save the result into the current answer slot.", if (sum_path) "add it to total" else "compare it with total and keep the larger value"),
      switch(level, "Return answer after the loop so earlier prefixes are retained.",
             "At a missing element, copy the previous total into that slot. Resume from the same total.",
             sprintf("At a missing element, store %s and restart the accumulation from there.", start)),
      sprintf("Trace the first two inputs, %s and %s. The first two saved totals should be %s and %s.", core_num(values[1]), core_num(values[2]), core_num(values[1]), core_num(second))),
    paste("u′ =", rule, "  (finite input step)"), symbols)
}

core_wrangle_notes <- function(ex, ctx) {
  df <- ex$inputs$df
  level <- ex$level
  if (ex$id == "filter_rows") {
    predicate <- sub(".*condition is true: (.*)\\. Preserve.*", "\\1", ex$instructions)
    op <- regmatches(predicate, regexpr(">=|<=|>|<", predicate))
    row <- if (level == 3L) which(is.na(df[[ctx$x]]))[1] else which(!is.na(df[[ctx$x]]))[1]
    v <- df[[ctx$x]][row]
    test <- switch(op, ">" = v > ctx$cut, ">=" = v >= ctx$cut, "<" = v < ctx$cut, "<=" = v <= ctx$cut)
    if (level >= 2L) test <- test & df[[ctx$g]][row] != "West"
    if (level == 3L) test <- test | is.na(v)
    return(guided_notes("Filtering keeps whole records. A missing logical result cannot establish that a row satisfies the condition.",
      c(sprintf("Put the comparison %s inside filter().", predicate),
        switch(level, "Place it inside filter(). Rows with a missing measurement drop out because the comparison is unknown.",
               sprintf("Combine the numeric test with the %s restriction using &. Check that both are true on a kept row.", ctx$g),
               sprintf("Group the two required tests in parentheses, then use | is.na(%s) to include missing measurements explicitly.", ctx$x)),
        sprintf("Input row %d has %s = %s%s, so its condition is %s. Keep all original columns on surviving rows.",
                row, ctx$x, core_num(v), if (level >= 2L) paste0(" and ", ctx$g, " = ", df[[ctx$g]][row]) else "", if (isTRUE(test)) "TRUE" else "FALSE")),
      "b = 𝟙{row condition evaluates to TRUE}",
      list(core_output("b", "Row-retention flag implemented by filter(). FALSE and NA conditions both yield zero."),
           core_fn("𝟙{·}", "Indicator that a predicate result is exactly TRUE.", "One logical result, including NA → {0, 1}", "The displayed predicate is fixed."))))
  }
  if (ex$id == "select_columns") {
    fields <- if (level >= 2L) ex$inputs$keep else trimws(strsplit(sub(".*in this order: (.*)\\. Keep.*", "\\1", ex$instructions), ",")[[1]])
    return(guided_notes("Column selection changes a table's schema while each record keeps its place.",
      c(sprintf("The first requested field here is %s. List the requested names in their specified order.", fields[1]),
        if (level >= 2L) "Pass keep to all_of() inside select(). New fixtures can supply different names and a different order."
        else "Pass the literal column names to select(). Its argument order becomes the output order.",
        if (level == 3L) sprintf("Use rename_with() on output position 1 to obtain primary_%s in this example.", fields[1])),
      sprintf("n′ = n = %d    c′ = %d", nrow(df), length(fields)),
      list(core_nrow(df), core_output("n′", "Number of records after select(), nrow(result)."),
           core_output("c′", "Output column count, length of the requested name list."))))
  }
  if (ex$id == "arrange_rows") {
    field <- if (level == 1L) ctx$y else ctx$x
    top <- max(df[[field]], na.rm = TRUE)
    return(guided_notes("A later sorting key can move a row only among rows tied on every earlier key.",
      c(sprintf("Sort %s descending. Its largest observed value is %s, so a row with that value must come first.", field, core_num(top)),
        switch(level, "Keep the original ordering among ties. arrange(desc(...)) is stable on this local table.",
               sprintf("Use %s ascending to break ties. Missing values of the primary key follow all observed values.", ctx$key),
               sprintf("For ties use %s ascending, then %s ascending. Leave only fully tied rows in their original order.", ctx$g, ctx$key))),
      sprintf("v₁ = max(𝐯) = %s", core_num(top)),
      list(c("𝐯", sprintf("Observed entries in df$%s, omitting NA for this calculation.", field), sprintf("%d × 1 column vector", sum(!is.na(df[[field]]))), "Observed values", "The input table and primary key are fixed."),
        core_fn("max", "Maximum of observed primary-key values, explaining the first position after desc().", "Nonempty numeric vector → numeric scalar", "Missing values are excluded from this check."),
        core_output("v₁", "First primary-key value in the sorted table. The subscript denotes the first output position."))))
  }
  if (ex$id == "filter_within_group" || ex$id == "centered_groups" || ex$id == "grouped_summary") {
    # Pick a nonmissing witness. A membership check still counts any missing rows.
    present <- which(!is.na(df[[ctx$x]]))
    first <- present[1]
    if (ex$id == "grouped_summary") {
      # A mixed group makes the difference between n() and a mean's observed
      # denominator visible. Prefer two observed entries for a nontrivial check.
      example_score <- vapply(present, function(row) {
        members <- df[[ctx$x]][core_group_rows(df, ctx, level, row)]
        2L * as.integer(sum(!is.na(members)) >= 2L) + as.integer(anyNA(members))
      }, 0L)
      first <- present[which.max(example_score)]
    }
    rows <- core_group_rows(df, ctx, level, first)
    key <- core_group_name(df, ctx, level, first)
    observed <- stats::na.omit(df[[ctx$x]][rows])
    stat <- core_summary_name(ex)
    center <- switch(stat, mean = mean(observed), median = stats::median(observed), sum = sum(observed))
    symbol <- if (stat == "mean") "v̄" else "s"
    meaning <- switch(stat,
      mean = sprintf("Within-group sample average, mean(%s, na.rm = TRUE).", ctx$x),
      median = sprintf("Within-group sample median (50th percentile), median(%s, na.rm = TRUE).", ctx$x),
      sum = sprintf("Within-group sum, sum(%s, na.rm = TRUE).", ctx$x))
    about <- sprintf("For %s, %d of %d values of %s are observed. Their %s is %s.", key, length(observed), length(rows), ctx$x, stat, core_num(center))
    symbols <- list(c("𝐯", sprintf("Observed %s values in this group, the values used with na.rm = TRUE.", ctx$x),
                      sprintf("%d × 1 column vector", length(observed)), "Observed values", paste("For the group with", key)),
                    core_fn(stat, paste0(stat, "(..., na.rm = TRUE) reduces these observations."),
                            "Nonempty numeric vector → one numeric scalar", "These group keys and observed entries are fixed."),
                    core_output(symbol, meaning, paste("For the group with", key)))
    formula <- sprintf("%s = %s(𝐯) ≈ %s", symbol, stat, core_num(center))
    if (ex$id == "grouped_summary") {
      arithmetic <- switch(stat,
        mean = sprintf("Add the observed entries to get %s, then divide by %d observed entries. This gives about %s.",
                       core_num(sum(observed)), length(observed), core_num(center)),
        sum = sprintf("Adding the observed entries gives %s = %s.", paste(core_num(observed), collapse = " + "), core_num(center)),
        median = {
          middle <- sort(observed)[unique(c(floor((length(observed) + 1) / 2), ceiling((length(observed) + 1) / 2)))]
          sprintf("In order the observed values are %s. %s", paste(core_num(sort(observed)), collapse = ", "),
                  if (length(middle) == 1L) sprintf("The middle value is %s.", core_num(center))
                  else sprintf("Average the two middle values, %s and %s, to get %s.", core_num(middle[1]), core_num(middle[2]), core_num(center)))
        })
      return(guided_notes("Grouping defines the records that belong together. Summarising then replaces each group with a single row.",
        c(about, arithmetic, sprintf("Group by %s before calling summarise(). Give the reduction the name requested in the task.", paste(c(ctx$g, if (level >= 2L) ctx$h), collapse = " and ")),
          if (level == 3L) sprintf("Use n() for records. The example group has %d total rows and %d observed measurements.", length(rows), length(observed)),
          'Specify .groups = "drop" to return the ungrouped result.'), formula, symbols))
    }
    v <- df[[ctx$x]][first]
    symbols <- c(symbols, list(core_input("v", sprintf("A single observed %s in the example group.", ctx$x))))
    if (ex$id == "centered_groups") {
      return(guided_notes("A centered value is a signed distance from a group's own center. mutate() keeps one result for each original row.",
        c(about, sprintf("Subtract %s from the observed value %s. This row's centered value is %s.", core_num(center), core_num(v), core_num(v - center)),
          if (level == 3L) sprintf("Repeat the calculation for %s using its own within-group center.", ctx$y),
          "Ungroup after adding the centered column or columns."),
        paste0(formula, "    d = v − ", symbol), c(symbols, list(core_output("d", "Centered measurement produced by mutate().")))))
    }
    extra <- if (level == 3L) ctx$by else 0
    threshold <- center + extra
    return(guided_notes("A group-relative filter compares each row with a threshold computed from its own peers.",
      c(about, sprintf("Compare %s against %s using strict >. %s", core_num(v), core_num(threshold),
                      if (v > threshold) "This example row survives." else "This example row drops out."),
        if (level == 3L) sprintf("Add the %d-unit margin to the group summary before comparing.", extra),
        "Ungroup the retained rows. Missing measurements cannot pass a strict comparison."),
      paste0(formula, "    keep when v > ", symbol, if (level == 3L) paste0(" + ", extra)), symbols))
  }
  if (ex$id == "mutate_measure" || ex$id == "across_transform") {
    first <- which(!is.na(df[[ctx$x]]))[1L]
    v <- df[[ctx$x]][first]
    second <- df[[ctx$y]][first]
    centered <- ex$id == "across_transform" && level == 3L
    offset <- if (centered) mean(df[[ctx$x]], na.rm = TRUE) else 0
    answer <- (v - offset) / ctx$by
    expr <- if (centered) "(v − v̄) / a" else "v / a"
    symbols <- list(core_input("v", sprintf("One observed value from %s.", ctx$x)), core_fixed("a", sprintf("Divisor %d in the transformation.", ctx$by)))
    if (centered) symbols <- c(symbols, list(core_output("v̄", sprintf("Observed mean of the current column, mean(value, na.rm = TRUE) = %s.", core_num(offset)))))
    if (ex$id == "mutate_measure" && level >= 2L) {
      answer <- answer + second
      expr <- paste(expr, "+ w")
      symbols <- c(symbols, list(core_input("w", sprintf("Value of %s from the same row.", ctx$y))))
      if (level == 3L) {
        answer <- round(answer, 1)
        expr <- paste0("round(", expr, ", 1)")
        symbols <- c(symbols, list(core_fn("round", "round(..., 1) rounds the transformed measurement.", "Numeric scalar → numeric scalar", "One decimal place.")))
      }
    }
    if (ex$id == "mutate_measure") {
      steps <- c(sprintf("Start with the observed %s value %s and divide it by %d.", ctx$x, core_num(v), ctx$by),
                 if (level >= 2L) sprintf("Add %s from %s on this same row.", core_num(second), ctx$y),
                 if (level == 3L) "Round the final sum to one decimal place.",
                 sprintf("Use mutate() to append adjusted_%s. Keep the original measurement as a separate column.", ctx$x))
    } else {
      steps <- c(sprintf("Select %s and %s together inside across(). The function receives each column in turn.", ctx$x, ctx$y),
                 switch(level, "Return the scaled vector to its original column name.",
                        'Use .names = "scaled_{.col}" to keep the originals alongside the new values.',
                        'Center each column on its own observed mean before dividing. Use .names = "scaled_{.col}" for the new columns.'),
                 sprintf("For example, the transformed %s corresponding to %s is %s.", ctx$x, core_num(v), core_num(answer)))
    }
    return(guided_notes(if (ex$id == "mutate_measure") "A new variable combines quantities belonging to the same record."
                        else "across() repeats one column operation without mixing the measurements from different fields.",
                        steps, sprintf("u = %s ≈ %s", expr, core_num(answer)), c(symbols, list(core_output("u", "Transformed value written into the output column.")))))
  }
  if (ex$id == "classify_values") {
    upper <- ctx$cut + ctx$by
    boundary_label <- if (level == 1L) "high" else "mid"
    return(guided_notes("The first true case wins. Order overlapping tests from the most specific low boundary upward.",
      c(sprintf("Handle is.na(%s) first and assign unknown.", ctx$x),
        sprintf("Test the strict lower boundary %d. A value equal to %d reaches the %s branch.", ctx$cut, ctx$cut, boundary_label),
        if (level >= 2L) sprintf("Use the next strict comparison at %d for the mid band.", upper),
        if (level == 3L) sprintf("Only remaining high-band rows reach the %s review check. Keep high as the fallback.", ctx$y)),
      sprintf('h(%d%s) = "%s"', ctx$cut, if (level == 3L) ", 0" else "", boundary_label),
      list(core_fn("h", "The case_when() classification rule inside mutate().",
                   if (level == 3L) "Pair of numeric values (missing permitted) → character label" else "Numeric value (missing permitted) → character label",
                   "The requested cutoffs, label definitions and order of cases are fixed."))))
  }
  # Both count exercises return one row per key rather than one row per record.
  rows <- core_group_rows(df, ctx, level)
  key <- core_group_name(df, ctx, level)
  weighted <- ex$id == "count_empty_groups" && level == 3L
  count <- if (weighted) sum(df[[ctx$y]][rows]) else length(rows)
  set <- c("𝒮", "Set of row positions matching this key, the rows that count() groups together.",
           sprintf("Subset of input positions, here containing %d positions", length(rows)), "Deterministic set", paste("For the group with", key))
  symbols <- list(set, core_output("c", "Group total returned in the named count column."))
  if (weighted) {
    symbols <- c(symbols, list(c("𝐰", sprintf("Weights taken from %s for the rows of 𝒮, passed through wt.", ctx$y),
                                 sprintf("%d × 1 column vector", length(rows)), "Observed values", "The group membership is fixed."),
                              core_fn("sum", "Reduction used by count(wt = ...).", "Numeric vector, including empty → numeric scalar", "The group's weights are fixed.")))
  } else symbols <- c(symbols, list(core_fn("|·|", "Cardinality, equivalent to n() within the group.", "Finite set → nonnegative integer", "The row membership of this group is fixed.")))
  guided_notes(if (ex$id == "count_empty_groups") "Factor levels define possible categories, even when a category has no records."
               else "Group frequency counts rows, so a blank measurement still contributes one observation.",
    c(sprintf("Build groups from %s. For %s, there are %d matching rows.", paste(c(ctx$g, if (level >= 2L) ctx$h), collapse = " and "), key, length(rows)),
      if (ex$id == "count_empty_groups") "Use .drop = FALSE so the listed but unused levels appear with totals of zero."
      else "Set name in count() to match the requested output column.",
      if (weighted) sprintf("Pass %s as wt. Summing its values gives %s for this group.", ctx$y, core_num(count)),
      if (ex$id == "count_groups" && level == 3L) "Sort by descending count and then the grouping keys ascending to settle ties."),
    sprintf("c = %s = %s", if (weighted) "sum(𝐰)" else "|𝒮|", core_num(count)), symbols)
}

core_table_notes <- function(ex, ctx) {
  level <- ex$level
  df <- ex$inputs$df
  if (startsWith(ex$id, "join_")) {
    lookup <- ex$inputs$lookup
    foreign <- if (level == 1L) ctx$key else "lookup_id"
    matches <- vapply(seq_len(nrow(df)), function(row) {
      keep <- lookup[[foreign]] == df[[ctx$key]][row]
      if (level == 3L) keep <- keep & lookup$lookup_period == df[[ctx$h]][row]
      sum(keep)
    }, 0L)
    hit <- ex$id == "join_inner"
    row <- which(if (hit) matches > 0L else matches == 0L)[1]
    key <- as.character(df[[ctx$key]][row])
    if (level == 3L) key <- paste0(key, " at ", df[[ctx$h]][row])
    effect <- switch(ex$id,
      join_left = "The incoming row stays in the result with a missing lookup label.",
      join_inner = "The incoming row stays, carrying the matching lookup's label.",
      join_unmatched = "This row belongs in the result because the lookup has no matching key.")
    symbols <- list(core_output("m", "Number of lookup rows with the required key for one incoming record.",
                               "This left row, lookup table and the named key mapping are fixed."),
                    core_output("c", "Number of output rows contributed by this left row.", "Its number of lookup matches and the selected join are fixed."))
    expr <- switch(ex$id, join_left = "max(1, m)", join_inner = "m", join_unmatched = "𝟙{m = 0}")
    if (ex$id == "join_left") symbols <- c(symbols, list(core_fn("max", "At least one output row per left row under left_join().", "Two integer counts → one integer count", "The join type is fixed.")))
    if (ex$id == "join_unmatched") symbols <- c(symbols, list(core_fn("𝟙{·}", "anti_join() keeps exactly the left rows without matches.", "Truth value → {0, 1}", "The membership test uses the specified keys.")))
    return(guided_notes(switch(ex$id,
      join_left = "A left join adds lookup information while preserving every incoming record.",
      join_inner = "An inner join builds records only from key matches between the two tables.",
      join_unmatched = "An anti join is a membership audit. Its output contains original rows whose keys are absent from the lookup."),
      c(sprintf("Match df$%s to lookup$%s. %s", ctx$key, foreign,
                if (level == 1L) "The key name is shared." else "Name both sides of the mapping explicitly."),
        if (level == 3L) sprintf("Include df$%s and lookup$lookup_period as a second key. An ID can appear under two different periods.", ctx$h),
        sprintf("For incoming row %d the key is %s and there are %d matches. %s", row, key, matches[row], effect)),
      sprintf("c = %s = %d   (worked row)", expr, if (hit) matches[row] else 1L), symbols))
  }
  if (ex$id == "drop_missing") {
    cols <- c(ctx$x, if (level >= 2L) ctx$y, if (level == 3L) ctx$g)
    rows <- stats::complete.cases(df[cols])
    witness <- which(!rows)[1L]
    flags <- as.integer(!is.na(unlist(df[witness, cols], use.names = FALSE)))
    return(guided_notes("Completeness is relative to the fields a task actually needs. A row can be useful even if unrelated metadata is missing.",
      c(sprintf("Give drop_na() exactly %s.", paste(cols, collapse = ", ")),
        sprintf("Check input row %d. For these fields its observed flags in order are %s, so the row drops out.", witness, paste(flags, collapse = ", ")),
        sprintf("Leave the remaining columns in place. This example keeps %d of %d rows.", sum(rows), nrow(df))),
      "bᵢ = ∏ₖ rᵢₖ",
      list(core_scalar("i", "Input row position inspected by drop_na().", "Unit index", sprintf("Ranges over the %d input rows.", nrow(df))),
           core_scalar("k", "Position in the list of required columns.", "Variable index", sprintf("Ranges over %d required fields in the specified order.", length(cols))),
           core_scalar("rᵢₖ", "Realized observation indicator for this cell, as.integer(!is.na(value)). One means observed.", "Observed indicator", "Given row i, required field k and the realized input."),
           core_fn("∏ₖ", "Product across the required observation flags. It is one only if they all equal one.", "Finite nonempty sequence of binary flags → {0, 1}", "The required fields and current row are fixed."),
           core_output("bᵢ", "Whether drop_na() retains row i as a complete record."))))
  }
  if (ex$id == "pivot_long") {
    cols <- names(df)[2:3]
    omitted <- sum(is.na(df[cols]))
    size <- 2L * nrow(df) - if (level >= 2L) omitted else 0L
    symbols <- list(core_nrow(df), core_output("n′", "Number of output measurement rows, nrow(result)."))
    if (level >= 2L) symbols <- c(symbols, list(core_output("m", sprintf("Count of missing cells removed by values_drop_na across %s.", paste(cols, collapse = " and ")))))
    return(guided_notes("A wide record stores measurements side by side. In long form, the field name travels with each measurement as data.",
      c(sprintf("Identify %s as the record key. Stack %s and %s with pivot_longer().", names(df)[1], cols[1], cols[2]),
        sprintf("The %d records offer %d possible name/value pairs. %s", nrow(df), 2L * nrow(df),
                if (level == 1L) "Keep even the pairs whose measurement is missing." else sprintf("Discard the %d pairs with missing measurements.", omitted)),
        if (level == 3L) "Use names_prefix to strip reading_ from labels while leaving the actual values untouched."),
      sprintf("n′ = 2n%s = %d", if (level >= 2L) " − m" else "", size), symbols))
  }
  id <- df[[1]]
  metric <- df[[2]]
  reading <- df[[3]]
  if (level == 3L) {
    duplicate <- which(duplicated(df[1:2]))[1]
    readings <- reading[id == id[duplicate] & metric == metric[duplicate]]
    return(guided_notes("A single wide cell has room for one number. Repeated measurements need an explicit reduction before they can occupy that cell.",
      c(sprintf("Use %s to identify rows, %s for names_from and %s for values_from.", names(df)[1], names(df)[2], names(df)[3]),
        sprintf("For key %s and field %s, the recorded values are %s. values_fn = mean reduces these to %s.",
                id[duplicate], metric[duplicate], paste(core_num(readings), collapse = ", "), core_num(mean(readings))),
        "Use values_fill = 0 for absent combinations. An explicit missing entry remains missing under this mean."),
      sprintf("z = mean(𝐯) ≈ %s", core_num(mean(readings))),
      list(c("𝐯", sprintf("The entries of %s falling into this one identifier/field cell.", names(df)[3]), sprintf("%d × 1 column vector", length(readings)), "Observed input entries", "The selected identifier and field are fixed."),
           core_fn("mean", "The aggregation supplied in values_fn.", "Nonempty numeric vector with possible NA → one value or NA", "Use all entries in the cell, preserving missingness."),
           core_output("z", "The single value assigned to that output cell."))))
  }
  absent <- 2L * length(unique(id)) - nrow(unique(df[1:2]))
  guided_notes("A wide table gives each identifier its own row and each measurement label its own column.",
    c(sprintf("Pass %s to names_from and %s to values_from. Leave %s as the identifier.", names(df)[2], names(df)[3], names(df)[1]),
      sprintf("There are %d identifiers and two measurement names. %d identifier/field pairs never appear.", length(unique(id)), absent),
      if (level == 1L) "Allow the unobserved pairs to become missing cells." else "Set values_fill = 0. It fills an absent pair while preserving a missing value explicitly recorded for a pair."),
    sprintf("a = 2 × c − b = %d", absent),
    list(core_output("c", sprintf("Number of distinct %s keys retained by pivot_wider().", names(df)[1])),
         core_output("b", "Count of distinct input key pairs, nrow(distinct(df, identifier, field)) with the actual column names."),
         core_output("a", "Number of absent identifier/field combinations that require filling.")))
}

core_graphics_notes <- function(ex, ctx) {
  df <- ex$inputs$df
  level <- ex$level
  if (ex$id == "bar_counts") {
    group <- as.character(df[[ctx$g]][1])
    part <- df[[ctx$h]][1]
    total <- sum(df[[ctx$g]] == group)
    cnt <- if (level >= 2L) sum(df[[ctx$g]] == group & df[[ctx$h]] == part) else total
    symbols <- list(core_output("c", "Count of records in the illustrated bar or fill segment, computed by geom_bar().",
                               "The displayed category and any mapped fill category are fixed."),
                    core_output("h", "Vertical height of the bar or its illustrated segment."))
    if (level == 3L) symbols <- c(symbols, list(core_output("t", "Count of all input rows in this horizontal category, the denominator for position = 'fill'.")))
    return(guided_notes("A bar made from raw records counts rows in a category. Fill groups subdivide the same horizontal category.",
      c(sprintf("Map %s to x. In this example %s contributes %d records.", ctx$g, group, total),
        if (level >= 2L) sprintf("Map factor(%s) to fill. For %s = %s the segment contains %d of those records.", ctx$h, ctx$h, part, cnt)
        else "Add geom_bar() and let its default count statistic compute the height.",
        if (level == 3L) sprintf('Use position = "fill" to divide segment counts by %d. Its height is about %s.', total, core_num(cnt / total))),
      if (level == 3L) sprintf("h = c / t ≈ %s", core_num(cnt / total)) else sprintf("h = c = %d", cnt), symbols))
  }
  row <- which(stats::complete.cases(df[c(ctx$x, ctx$y)]))[1L]
  u <- df[[ctx$x]][row]
  v <- df[[ctx$y]][row]
  axes <- sprintf("Input row %d gives the position (%s, %s). Use %s for x and %s for y.", row, core_num(u), core_num(v), ctx$x, ctx$y)
  if (ex$id == "scale_theme") {
    horizontal <- grepl("scale_x_log10", ex$solution, fixed = TRUE)
    value <- if (horizontal) u else v
    field <- if (horizontal) ctx$x else ctx$y
    return(guided_notes("A scale changes the mapping from measurements to positions. Tick labels can still be read in original units.",
      c(axes, sprintf("Apply the log scale to the %s axis. A value of %s is plotted at approximately %s in log coordinates.", if (horizontal) "horizontal" else "vertical", core_num(value), core_num(log10(value))),
        if (level >= 2L) "Add the named complete theme after the plot layers to apply its non-data styling.",
        if (level == 3L) "Add the square-root scale to the other axis. Leave the point geometry unchanged."),
      sprintf("z = log₁₀(v) ≈ %s", core_num(log10(value))),
      list(core_input("v", sprintf("One positive value of %s, here %s.", field, core_num(value))),
           core_fn("log₁₀", "Base-ten logarithm in scale_x_log10() or scale_y_log10() on the requested axis.", "Positive real scalar → real scalar", "The base is fixed at ten."),
           core_output("z", "Transformed coordinate for that measurement, before mapping onto the panel."))))
  }
  coords <- list(core_input("u", sprintf("Value of %s mapped horizontally in aes().", ctx$x)), core_input("v", sprintf("Value of %s mapped vertically in aes().", ctx$y)),
                 c("𝐜", "One plotted point's coordinate pair arranged as a column.", "2 × 1 column vector", "Deterministic mapping", "The observed row and the two aesthetic mappings are fixed."),
                 core_fn("ᵀ", "Transpose, writing the displayed row pair as a column.", "1 × 2 row vector → 2 × 1 column vector", "The two coordinate entries are fixed."))
  if (ex$id == "facet_scatter") {
    keys <- core_group_name(df, ctx, level, row)
    return(guided_notes("A facet partitions records into panels. The two numeric variables retain the same meaning within every panel.",
      c(axes, sprintf("This point belongs in the panel for %s. Put the required panel variable%s inside facet_wrap(vars(...)).", keys, if (level >= 2L) "s" else ""),
        if (level == 3L) 'Use scales = "free_y". This changes panel-specific vertical ranges while retaining the original coordinate values.'
        else "Keep the default shared axes so the same numeric distance has the same visual scale across panels."),
      sprintf("𝐜 = (u, v)ᵀ = (%s, %s)ᵀ", core_num(u), core_num(v)), coords))
  }
  guided_notes("Each complete row supplies two coordinates for one point. Additional aesthetics describe that same row.",
    c(axes, "Add one geom_point() layer and return the plot object.",
      if (level >= 2L) sprintf("Map %s inside aes(colour = ...). For this point the colour category is %s.", ctx$g, df[[ctx$g]][row]),
      if (level == 3L) sprintf("Map factor(%s) to shape so discrete categories get distinct shapes.", ctx$h)),
    sprintf("𝐜 = (u, v)ᵀ = (%s, %s)ᵀ", core_num(u), core_num(v)), coords)
}

core_descriptive_notes <- function(ex, ctx) {
  df <- ex$inputs$df
  labels <- regmatches(ex$solution, gregexpr('"[^"]+"', ex$solution))[[1L]]
  labels <- gsub('"', "", labels, fixed = TRUE)
  values <- function(label = NULL) {
    column <- df[[ctx$x]]
    if (!is.null(label)) column <- column[df[[ctx$g]] == label]
    column[!is.na(column)]
  }
  v <- if (!length(labels)) values() else values(labels[1])
  stat <- if (startsWith(ex$solution, "sd(")) "sd" else if (startsWith(ex$solution, "IQR(")) "IQR" else "median"
  summarize <- switch(stat, sd = stats::sd, IQR = stats::IQR, median = stats::median)
  result <- summarize(v)
  where <- if (length(labels)) paste("Rows with", ctx$g, "equal to", labels[1]) else "The entire worked table"
  condition <- paste0(where, ", excluding missing ", ctx$x, " values.")
  symbols <- list(core_output("s", sprintf("Worked %s of %s, computed with na.rm = TRUE.", stat, ctx$x), condition))
  if (stat == "sd") {
    math <- "s = √[∑ᵢ (yᵢ − ȳ)² / (n − 1)]"
    symbols <- c(symbols, list(
      core_scalar("n", sprintf("Number of observed values entering sd(), here %d.", length(v)), "Observed count", condition),
      core_scalar("i", "Position of an observation in this input to sd().", "Unit index", "Ranges from 1 through n within the selected sample."),
      core_scalar("yᵢ", sprintf("Observed %s value at position i.", ctx$x), "Observed realization", condition),
      core_output("ȳ", "Arithmetic average of the values entering sd().", condition),
      core_fn("∑ᵢ", "Sum across the observed units in the sample.", "n numeric addends → one numeric scalar", condition),
      core_fn("√", "Principal square root converting the sample variance into sd().", "Nonnegative numeric scalar → nonnegative numeric scalar", condition)))
    step <- if (length(v) < 2L) "Only one value is available. sd() returns NA because the sample variance divisor is zero."
            else sprintf("Their mean is %s. Square the deviations from this mean, divide their sum by %d, then take the square root.", core_num(mean(v)), length(v) - 1L)
    if (length(v) < 2L) math <- paste(math, "(requires n > 1, otherwise sd() returns NA)")
  } else if (stat == "IQR") {
    math <- "s = q̂₀.₇₅ − q̂₀.₂₅"
    q <- as.numeric(stats::quantile(v, c(.25, .75), names = FALSE))
    step <- sprintf("Using R's default interpolation, the quartiles are %s and %s. Subtract the lower quartile from the upper.", core_num(q[1]), core_num(q[2]))
    symbols <- c(symbols, list(core_output("q̂₀.₇₅", "Estimated 75th percentile, the upper sample quartile used by IQR(type = 7).", condition),
                               core_output("q̂₀.₂₅", "Estimated 25th percentile, the lower sample quartile used by IQR(type = 7).", condition)))
  } else {
    math <- "s = q̂₀.₅"
    step <- "Order the observed values. Take the central value for an odd count or average the two central values for an even count."
    symbols <- c(symbols, list(core_output("q̂₀.₅", "Estimated 50th percentile, the sample median returned by median().", condition)))
  }
  steps <- c(sprintf("Use %s. The calculation includes %d observed %s values.", if (length(labels)) paste("the rows with", ctx$g, "equal to", labels[1]) else "the entire worked table", length(v), ctx$x), step,
             sprintf("The resulting %s for this sample is %s.", stat, core_num(result)))
  if (ex$level == 3L) {
    other <- summarize(values(labels[2]))
    steps <- c(steps, sprintf("Repeat within %s to obtain %s. Subtract that second statistic from the first to return %s.",
                              labels[2], core_num(other), core_num(result - other)))
    math <- paste0(math, "  (apply within each group)    d = s₁ − s₂")
    symbols <- c(symbols, list(core_output("s₁", sprintf("First %s in the difference, for %s.", stat, labels[1]), condition),
                               core_output("s₂", sprintf("Second %s in the difference, for %s.", stat, labels[2]), paste("The second group and its observed measurements are fixed.")),
                               core_output("d", "Requested signed difference returned by the final subtraction.")))
  }
  guided_notes("A descriptive statistic is meaningful only after deciding which measurements belong in its calculation.", steps, math, symbols)
}

core_stat_notes <- function(ex, ctx) {
  level <- ex$level
  inputs <- ex$inputs
  if (ex$id == "descriptive_statistic") return(core_descriptive_notes(ex, ctx))
  if (ex$id == "normal_probability") {
    spread <- if (level == 1L) inputs$spread else sqrt(inputs$variance_value)
    upper <- grepl("lower.tail = FALSE", ex$solution, fixed = TRUE)
    bounds <- level == 3L
    symbols <- list(
      core_scalar("Y", "A future scalar Normal draw whose probability pnorm() evaluates.", "Random variable", "Conditional on the supplied fixed Normal mean and variance."),
      core_fixed("μ", sprintf("Normal mean, center = %s.", core_num(inputs$center))),
      core_scalar("σ", sprintf("Positive Normal standard deviation passed to sd, here %s.", core_num(spread)),
                  if (level == 1L) "Fixed supplied parameter" else "Deterministic positive square root", if (level == 1L) "The prompt supplies spread." else "The supplied variance is held fixed."),
      core_scalar("σ²", sprintf("Normal variance, %s = %s.", if (level == 1L) "spread^2" else "variance_value", core_num(spread^2)),
                  if (level == 1L) "Deterministic square of the fixed SD" else "Fixed supplied parameter", "The exercise specifies the Normal law."),
      core_fn("F_Y", "Conditional CDF of Y, pnorm(..., mean = center, sd = the standard deviation).", "Real cutoff → probability in [0, 1]", "The supplied mean μ and variance σ² are fixed."),
      core_output("π", "Event probability returned for the illustrated cutoff or interval.", "The fixed Normal law and event boundaries are given."))
    if (bounds) {
      lo <- inputs$lower_bound[1]
      hi <- inputs$upper_bound[1]
      answer <- stats::pnorm(hi, inputs$center, spread) - stats::pnorm(lo, inputs$center, spread)
      symbols <- c(symbols, list(core_fixed("l", sprintf("First lower_bound, %s.", core_num(lo))), core_fixed("u", sprintf("Paired upper_bound, %s.", core_num(hi)))))
      desc <- sprintf("For the first pair [%s, %s], subtract the cumulative probability at the lower endpoint from that at the upper endpoint.", core_num(lo), core_num(hi))
      math <- "π = F_Y(u) − F_Y(l)"
    } else {
      cut <- inputs$cutoffs[1]
      answer <- stats::pnorm(cut, inputs$center, spread, lower.tail = !upper)
      symbols <- c(symbols, list(core_fixed("c", sprintf("First numeric cutoff, cutoffs[1] = %s.", core_num(cut)))))
      desc <- sprintf("For cutoff %s, use lower.tail = %s to compute the chance of a draw %s it.", core_num(cut), if (upper) "FALSE" else "TRUE", if (upper) "above" else "at or below")
      math <- if (upper) "π = 1 − F_Y(c)" else "π = F_Y(c)"
    }
    return(guided_notes("A cumulative probability is area accumulated from the left. Choosing a tail or subtracting two cumulative probabilities identifies the event's area.",
      c(if (level == 1L) sprintf("pnorm() takes a standard deviation. Supply spread, here %s, directly.", core_num(spread))
        else sprintf("Take the square root of variance_value = %s. The sd argument is %s.", core_num(inputs$variance_value), core_num(spread)),
        desc, sprintf("The first probability is about %s. Pass the entire cutoff or paired-bound vectors to obtain the answers in their original order.", core_num(answer))),
      paste(math, "≈", core_num(answer)), symbols))
  }
  if (ex$id == "simulation_rate") {
    draws <- inputs$center + inputs$spread * inputs$noise
    include <- if (level == 3L) draws > inputs$eligibility else rep(TRUE, length(draws))
    hit <- if (level > 1L) draws >= inputs$lower_bound & draws <= inputs$upper_bound
           else if (grepl("draws > cutoff", ex$solution, fixed = TRUE)) draws > inputs$cutoff else draws < inputs$cutoff
    events <- sum(hit & include)
    count <- sum(include)
    return(guided_notes("The supplied noise is already a simulation. Reusing its draws makes changes in the reported frequency traceable to the event you coded.",
      c(sprintf("First scale and shift. The first noise value of %s becomes a draw of approximately %s.", core_num(inputs$noise[1]), core_num(draws[1])),
        if (level == 1L) sprintf("Use the strict comparison in the prompt at cutoff %s.", core_num(inputs$cutoff))
        else sprintf("Flag draws on or between %s and %s. Both endpoints belong to the event.", core_num(inputs$lower_bound), core_num(inputs$upper_bound)),
        if (level == 3L) sprintf("Restrict the denominator as well as the numerator to draws above %s.", core_num(inputs$eligibility)),
        sprintf("The mean of the selected logical flags counts %d events in %d eligible draws, a fraction of approximately %s.", events, count, core_num(events / count))),
      "y = μ + σ × z    π̂ = h / e",
      list(core_scalar("z", "One realized standard-Normal draw in noise.", "Realized simulation value", "The supplied noise vector is fixed for grading."),
           core_fixed("μ", "Desired Normal mean, center."), core_fixed("σ", "Desired positive Normal standard deviation, spread."),
           core_output("y", "One transformed draw in draws.", "The realized z, mean and standard deviation are fixed."),
           core_output("h", "Number of eligible simulated draws meeting the event, the sum of the selected logical flags."),
           core_output("e", "Number of eligible draws in the denominator of mean()."),
           core_scalar("π̂", "Monte Carlo estimate of the requested event probability, the mean of those flags.", "Deterministic point summary of these realized draws", "The supplied simulation, task bounds and eligibility rule are held fixed."))))
  }
  df <- inputs$df
  predictors <- c(ctx$x, if (level == 3L) "exposure")
  estimate <- function(symbol, meaning) {
    core_scalar(symbol, meaning, "Realized frequentist coefficient estimate", "The observed df, fitted formula and estimator are fixed. The model coefficients are fixed unknown parameters.")
  }
  if (ex$id == "linear_slope") {
    fit <- stats::lm(stats::reformulate(predictors, response = ctx$y), data = df)
    slope <- unname(stats::coef(fit)[ctx$x])
    delta <- if (level == 1L) 1L else ctx$by
    return(guided_notes("In an additive linear model the fitted slope gives the change in the fitted conditional mean for a one-unit increase in that predictor.",
      c(sprintf("Fit lm() with %s as the response and %s as the predictor%s. Keep the intercept.", ctx$y, paste(predictors, collapse = " and "), if (level == 3L) "s" else ""),
        sprintf("Extract the coefficient named %s. It is about %s response units per predictor unit in this worked sample.", ctx$x, core_num(slope)),
        sprintf("Multiply that slope by the requested change of %d units. %s", delta, if (level == 3L) "Hold exposure fixed in the comparison." else "The same intercept occurs in both fitted means, so it drops out of their difference.")),
      sprintf("Δμ̂ = β̂₁ × d ≈ %s", core_num(slope * delta)),
      list(estimate("β̂₁", sprintf("Fitted slope, coef(fit)[['%s']]. The subscript identifies the first predictor.", ctx$x)),
           core_fixed("d", sprintf("Prescribed change of %d predictor units.", delta)),
           core_scalar("Δμ̂", "Difference between two fitted conditional means, slope times predictor change.", "Deterministic contrast of frequentist fitted values", "The observed fit and predictor change are fixed, as are any other predictor values."))))
  }
  target <- as.integer(sub("^.*equal to (?:c\\()?([0-9]+).*", "\\1", ex$instructions, perl = TRUE))
  fit <- stats::glm(stats::reformulate(predictors, response = "event"), data = df, family = stats::binomial())
  newrow <- setNames(data.frame(target), ctx$x)
  if (level == 3L) newrow$exposure <- 1
  logodds <- unname(stats::predict(fit, newrow))
  probability <- stats::plogis(logodds)
  symbols <- list(estimate("β̂₀", "Fitted intercept from glm(), the coefficient labeled (Intercept)."),
                  estimate("β̂₁", sprintf("Fitted coefficient on %s, the first predictor.", ctx$x)),
                  core_fixed("v", sprintf("New prediction value of %s, here %d in the first prediction row.", ctx$x, target)))
  if (level == 3L) symbols <- c(symbols, list(estimate("β̂₂", "Fitted coefficient on exposure, the second predictor."),
                                              core_fixed("w", "Exposure for the current newdata row, here 1.")))
  symbols <- c(symbols, list(core_output("η̂⁽ˡⁱⁿ⁾", "Fitted linear predictor on the log-odds scale, predict(fit, newdata, type = 'link').", "The frequentist fit and one row of newdata are fixed."),
                             core_fn("expit", "Inverse logit, plogis() or the inverse link in predict(..., type = 'response').", "Real log odds → probability in (0, 1)", "The binomial logit link is fixed."),
                             core_output("μ̂", "Estimated conditional mean of the binary event, the fitted event probability.", "The observed fit and predictor values in that prediction row are fixed.")))
  guided_notes("Logistic regression models log odds. A probability prediction applies the inverse logit after combining the intercept and predictor terms.",
    c(sprintf("Fit glm() to event with family = binomial() and predictor%s %s.", if (level == 3L) "s" else "", paste(predictors, collapse = " and ")),
      if (level == 1L) sprintf("Construct one newdata row with %s equal to %d.", ctx$x, target)
      else if (level == 2L) "Construct newdata with the three supplied predictor values in the requested order."
      else "Pair the three requested predictor values with exposures 1, 3 and 5 in order, with one pair per newdata row.",
      sprintf("For the first requested row the fitted log odds are about %s. type = \"response\" converts them to a probability of about %s.", core_num(logodds), core_num(probability))),
    paste0("η̂⁽ˡⁱⁿ⁾ = β̂₀ + β̂₁ × v", if (level == 3L) " + β̂₂ × w", "    μ̂ = expit(η̂⁽ˡⁱⁿ⁾)"), symbols)
}

guided_core_notes <- function(ex) {
  # Walkthrough values come from the displayed input and its template context.
  id <- ex$id
  builder <- if (id %in% c("vector_assignment", "vector_index", "conditional_function", "loop_accumulate")) {
    core_basics_notes
  } else if (id %in% c("filter_rows", "select_columns", "arrange_rows", "filter_within_group",
                       "mutate_measure", "classify_values", "across_transform", "grouped_summary",
                       "count_groups", "centered_groups", "count_empty_groups")) {
    core_wrangle_notes
  } else if (id %in% c("join_left", "join_inner", "join_unmatched", "drop_missing", "pivot_long", "pivot_wide")) {
    core_table_notes
  } else if (id %in% c("scatter_mapping", "bar_counts", "facet_scatter", "scale_theme")) {
    core_graphics_notes
  } else if (id %in% c("descriptive_statistic", "normal_probability", "simulation_rate", "linear_slope", "logistic_prediction")) {
    core_stat_notes
  } else return(NULL)
  builder(ex, ex$context)
}
