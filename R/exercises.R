# Each exercise_library_* function contributes named templates. Lookup happens
# at request time so an extension can be sourced in any order.
exercise_template <- function(category, title, difficulty, build) {
  list(category = category, title = title, difficulty = as.integer(difficulty), build = build)
}

exercise_templates <- function() {
  scope <- environment(exercise_templates)
  providers <- ls(scope, pattern = "^exercise_library_")
  templates <- do.call(c, unname(lapply(mget(providers, envir = scope), function(f) f())))
  if (!length(templates) || anyDuplicated(names(templates))) {
    stop("Exercise template IDs must exist and be unique.")
  }
  templates
}

exercise_catalog <- function() {
  templates <- exercise_templates()
  data.frame(
    id = names(templates),
    category = vapply(templates, `[[`, "", "category"),
    title = vapply(templates, `[[`, "", "title"),
    difficulty = vapply(templates, `[[`, 0L, "difficulty"),
    row.names = NULL
  )
}

exercise_context <- function() {
  measures <- sample(c("duration", "cost", "distance", "score", "volume", "weight"), 2)
  list(
    x = measures[1], y = measures[2],
    g = sample(c("region", "team", "site", "department", "district"), 1),
    h = sample(c("year", "cohort", "season"), 1),
    key = sample(c("item_id", "record_id", "unit_id"), 1),
    cut = sample(25:70, 1), by = sample(2:7, 1)
  )
}

exercise_frame <- function(ctx, variant) {
  size <- sample(13:29, 1)
  group <- sample(rep(c("North", "East", "South", "West"), length.out = size))
  measure <- sample(10:99, size, replace = TRUE) + sample(c(0, .5), size, replace = TRUE)
  measure[sample.int(size, 2)] <- NA_real_
  if (variant == 3L) measure[group == "South"] <- NA_real_
  df <- data.frame(
    id = seq_len(size), group = group, period = sample(2021:2023, size, TRUE),
    first = measure, second = sample(1:65, size, TRUE), stringsAsFactors = FALSE
  )
  names(df) <- c(ctx$key, ctx$g, ctx$h, ctx$x, ctx$y)
  list(df = df)
}

exercise_plan <- function(instructions, solution, concept, technique, partial,
                          explanation, contract = list(kind = "value"),
                          fixture = exercise_frame) {
  list(
    instructions = instructions, solution = solution,
    hints = c(concept, technique, partial, paste(solution, explanation, sep = "\n\n")),
    explanation = explanation, contract = contract, fixture = fixture
  )
}

exercise_table <- function(ordered = FALSE, column_order = FALSE, storage = NULL) {
  contract <- list(kind = "table", ordered = ordered, column_order = column_order)
  if (!is.null(storage)) contract$storage <- storage
  contract
}

make_exercise <- function(id, seed, level = 1L) {
  template <- exercise_templates()[[id]]
  if (is.null(template)) stop("Unknown exercise ID: ", id)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) ||
      seed != trunc(seed) || seed < 0 || seed > .Machine$integer.max) {
    stop("Seed must be a nonnegative R integer.")
  }
  if (length(level) != 1L || !level %in% 1:3) stop("Level must be 1, 2 or 3.")
  seed <- as.integer(seed)
  level <- as.integer(level)
  withr::with_seed(seed, {
    ctx <- exercise_context()
    spec <- template$build(ctx, level)
    fixtures <- lapply(seq_len(3L), function(i) spec$fixture(ctx, i))
    c(list(id = id, category = template$category, seed = seed, level = level,
           inputs = fixtures[[1L]], fixtures = fixtures),
      spec[c("instructions", "solution", "hints", "explanation", "contract")])
  })
}

exercise_library_grouped <- function() {
  list(grouped_summary = exercise_template(
    "Grouped operations", "Grouped summaries", 1, function(ctx, level) {
      stat <- sample(c("mean", "median", "sum"), 1)
      groups <- if (level >= 2L) c(ctx$g, ctx$h) else ctx$g
      keys <- paste(groups, collapse = ", ")
      name <- paste(stat, ctx$x, sep = "_")
      summary <- sprintf("%s = %s(%s, na.rm = TRUE)", name, stat, ctx$x)
      extra <- if (level >= 3L) ", records = n()" else ""
      task <- sprintf(
        "Using df, return one ungrouped row per observed combination of %s. Compute the %s of %s, excluding missing values, in %s.%s Return the grouping columns first.",
        keys, stat, ctx$x, name,
        if (level >= 3L) " Add records as the final column, counting every row in each group." else ""
      )
      exercise_plan(task,
        sprintf("df |>\n  group_by(%s) |>\n  summarise(%s%s, .groups = \"drop\")",
                keys, summary, extra),
        "Split the rows into groups before calculating the summary.",
        "Use group_by() followed by summarise().",
        sprintf("df |> group_by(%s) |> summarise(%s = ...)", keys, name),
        sprintf("Each group supplies its own %s values. The missing-value option excludes missing measurements from the %s.", ctx$x, stat),
        exercise_table(column_order = TRUE)
      )
    }
  ))
}
