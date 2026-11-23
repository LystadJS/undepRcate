# Adding an exercise

Put a provider function in a new file under `R/`. Its name begins with `exercise_library_` and it returns a named list of templates. Each list name is a permanent drill ID used in sessions, review schedules and history. Restart the application to load the provider. Its category and title appear in the drill selector automatically.

From the repository root, save this example as `R/exercises-text.R`.

```r
exercise_library_text <- function() {
  list(trim_labels = exercise_template(
    category = "R fundamentals", title = "Trim text labels", difficulty = 1L,
    build = function(ctx, level) {
      exercise_plan(
        instructions = paste(
          "Remove leading and trailing whitespace from every element of labels.",
          "Return a character vector in input order and preserve missing values."
        ),
        solution = "trimws(labels)",
        concept = "Clean each label separately and leave its internal spaces intact.",
        technique = "Use the vectorized function trimws().",
        partial = "trimws(...)",
        explanation = "trimws() removes whitespace at both ends and preserves internal spaces.",
        contract = list(kind = "value"),
        fixture = function(ctx, variant) {
          words <- sample(c("entry", "field note", "exit"), 8L, replace = TRUE)
          labels <- paste0(sample(c("", " ", "  "), 8L, TRUE), words,
                           sample(c("", " ", "\t"), 8L, TRUE))
          if (variant == 3L) labels[1L] <- NA_character_
          list(labels = labels)
        }
      )
    }
  ))
}
```

### How a variation is generated

`exercise_template()` records the category, title, base difficulty (1 through 3), and builder. The builder receives a shared context and the learner's current level (1 through 3). A builder can introduce a second grouping key or a more demanding output as the level rises. The context supplies randomized column names and constants for the built-in data-frame fixtures. Templates can supply their own fixtures, as the example does.

`exercise_plan()` describes one operation. It takes an instruction, executable reference solution, conceptual hint, technique hint, partial solution, explanation, contract and fixture generator. The fourth hint combines the full solution and explanation. State each output requirement in the instruction, including column order or grouping when either matters.

`make_exercise()` calls the builder once and its fixture generator three times within a saved random seed. Each fixture returns a **named list** of independent R inputs in the same schema. The first appears in the preview. The other two run the same submission on different data. Put missing values, repeated keys, boundary inputs or empty groups where they distinguish a correct operation from an accidental match. Use the supplied random state throughout. An unchanged template, seed and level regenerate the same exercise when a session resumes.

### How the answer is checked

The last expression in the submission supplies its answer. Assignments also return their value in R. Each fixture gets fresh bindings and the worker makes `dplyr`, `tidyr`, `purrr` and `ggplot2` available. Numerical comparisons use a tolerance of `1e-7`.

| Contract | What the worker checks |
| --- | --- |
| `list(kind = "value")` | The resulting R value, type, dimensions and names. Ordinary integer and double representations of the same numbers are accepted. |
| `exercise_table()` | Dimensions, column names and types, grouping, and all cell values including missing values. A data frame and a tibble can agree. Pass `ordered = TRUE` for required row order and `column_order = TRUE` for required column order. If a schema requires exact storage, declare it with e.g. `storage = c(visit = "integer")`. Use ordinary sortable columns for unordered comparisons. |
| `list(kind = "function", cases = list(list(0), list(NA_real_)))` | The result must be a function. The worker calls it and the reference on each argument list and compares their results. Supply at least two cases. Include boundary cases and use positional arguments when the prompt leaves argument names open. |
| `list(kind = "plot")` | Evaluated aesthetic mappings, geom, stat, position, built structural data, panel layout, coordinates, and trained axis and aesthetic scales. Add `default_style = TRUE` to check built layer styling, `theme = TRUE` to check the requested theme or `labels = c("title", "x")` to check specific labels. |

For a new output type that needs different semantics, add a contract and explicit worker tests before registering drills for it. Keep model-fitting exercises within the existing value contract by asking for a prediction or extracted coefficient.

### Try the example

In an R console from the repository root, after saving the provider, run

```r
source("scripts/load.R")
exercise <- make_exercise("trim_labels", seed = 84L)
exercise$inputs
assess_answer(exercise, "trimws(labels)")$status
assess_answer(exercise, "labels")$checks
```

The first submission should pass. The second leaves whitespace in place. Check an independent correct implementation and a plausible wrong one across several seeds. Run the complete test suite with `Rscript --vanilla tests/testthat.R` or the **undepRcate: Run tests** project task.
