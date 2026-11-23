# Loaded by absolute path into the disposable grading process only.
check_result <- function(label, passed, detail) {
  stopifnot(is.logical(passed), length(passed) == 1L, !is.na(passed))
  data.frame(label = label, passed = passed, detail = if (passed) "Passed" else detail)
}

same_value <- function(actual, expected) {
  tryCatch({
    testthat::expect_equal(actual, expected, tolerance = 1e-7)
    TRUE
  }, expectation_failure = function(e) FALSE)
}

column_kind <- function(x) {
  if (!is.object(x) && typeof(x) %in% c("integer", "double")) "numeric" else
    paste(typeof(x), paste(class(x), collapse = "/"))
}

sort_rows <- function(x) {
  if (nrow(x) > 1L && ncol(x) > 0L)
    x <- x[do.call(order, c(unname(as.list(x)), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}

table_groups <- function(x) {
  list(keys = dplyr::group_vars(x), rowwise = inherits(x, "rowwise_df"),
       groups = if (dplyr::is_grouped_df(x)) sort_rows(as.data.frame(dplyr::group_keys(x))),
       drop = dplyr::group_by_drop_default(x))
}

grade_table <- function(actual, expected, contract) {
  stopifnot(is.data.frame(expected), !anyDuplicated(names(expected)))
  storage <- contract$storage
  if (!is.null(storage)) {
    stopifnot(is.character(storage), length(storage) > 0L, !is.null(names(storage)),
              all(nzchar(names(storage))), !anyDuplicated(names(storage)), all(names(storage) %in% names(expected)),
              identical(vapply(expected[names(storage)], typeof, ""), storage))
  }
  checks <- check_result("Data frame", is.data.frame(actual), "Return a data frame or tibble.")
  if (!all(checks$passed)) return(checks)
  same_columns <- if (isTRUE(contract$column_order)) identical(names(actual), names(expected)) else
    !anyDuplicated(names(actual)) && setequal(names(actual), names(expected))
  checks <- rbind(checks,
    check_result("Dimensions", identical(dim(actual), dim(expected)),
                 sprintf("Expected %d rows and %d columns; got %d and %d.",
                         nrow(expected), ncol(expected), nrow(actual), ncol(actual))),
    check_result("Column names", same_columns,
                 paste("Expected", paste(names(expected), collapse = ", "),
                       "and received", paste(names(actual), collapse = ", "))),
    check_result("Grouping", same_value(table_groups(actual), table_groups(expected)),
                 "Check the grouping of the returned data frame."))
  if (!same_columns) return(checks)
  if (!is.null(storage)) {
    actual_types <- vapply(actual[names(storage)], typeof, "")
    wrong <- names(storage)[actual_types != storage]
    checks <- rbind(checks, check_result("Declared storage types", !length(wrong),
      paste(sprintf("%s requires %s, received %s.", wrong, storage[wrong], actual_types[wrong]), collapse = " ")))
  }
  actual <- as.data.frame(actual)[names(expected)]
  expected <- as.data.frame(expected)
  types <- vapply(actual, column_kind, "") == vapply(expected, column_kind, "")
  checks <- rbind(checks, check_result("Column types", all(types),
                        paste("Check the type of", paste(names(types)[!types], collapse = ", "))))
  if (!all(types) || nrow(actual) != nrow(expected)) return(checks)
  if (!isTRUE(contract$ordered) && nrow(expected) > 1L) {
    actual <- sort_rows(actual)
    expected <- sort_rows(expected)
  }
  values <- vapply(names(expected), function(nm) same_value(actual[[nm]], expected[[nm]]), logical(1))
  rbind(checks, check_result(if (isTRUE(contract$ordered)) "Values and row order" else "Values",
                 all(values), paste("Check the values of", paste(names(values)[!values], collapse = ", "))))
}

grade_function <- function(actual, expected, contract, run_student) {
  stopifnot(is.function(expected), is.list(contract$cases), length(contract$cases) >= 2L,
            all(vapply(contract$cases, is.list, logical(1))))
  checks <- check_result("Function object", is.function(actual), "Return a function.")
  if (!is.function(actual)) return(checks)
  for (case in seq_along(contract$cases)) {
    args <- contract$cases[[case]]
    target <- do.call(expected, args)
    observed <- run_student(do.call(actual, args))
    checks <- rbind(checks, check_result(paste("Function input", case),
      identical(column_kind(observed), column_kind(target)) && same_value(observed, target),
      "The function returned an incorrect value, type, or shape for this input."))
  }
  checks
}

# Inspect evaluated aesthetic values and layer data, never solution syntax or pixels.
plot_layer_signature <- function(built, i, default_style) {
  layer <- built$plot$layers[[i]]
  inputs <- layer$layer_data(built$plot$data)
  if (is.null(inputs)) inputs <- data.frame()
  inputs$PANEL <- rep(1L, nrow(inputs))
  aesthetics <- layer$compute_aesthetics(inputs, built$plot)
  aesthetics <- aesthetics[sort(setdiff(names(aesthetics), c("PANEL", "group")))]
  # A factor and a character column can encode precisely the same visual categories.
  # Scale training and the built data below preserve differences in their ordering.
  for (name in names(aesthetics)) if (is.factor(aesthetics[[name]]))
    aesthetics[[name]] <- as.character(aesthetics[[name]])
  output <- built$data[[i]]
  structural <- intersect(c("PANEL", "group", "x", "y", "xmin", "xmax", "ymin", "ymax",
                            "xend", "yend", "count", "density"), names(output))
  output <- output[if (default_style) sort(names(output)) else structural]
  if (inherits(layer$geom, c("GeomPoint", "GeomBar", "GeomLine"))) {
    aesthetics <- sort_rows(aesthetics)
    output <- sort_rows(output)
  }
  list(
    geom = class(layer$geom)[1L],
    stat = class(layer$stat)[1L],
    position = class(layer$position)[1L],
    mappings = sort(names(layer$computed_mapping)),
    aesthetics = aesthetics,
    output = output
  )
}

scale_signature <- function(scale) {
  transformation <- if (is.function(scale$get_transformation)) scale$get_transformation()$name else NULL
  list(type = class(scale)[1L], transform = transformation, limits = scale$get_limits(),
       breaks = scale$get_breaks(), labels = scale$get_labels())
}

plot_signature <- function(plot, contract) {
  built <- ggplot2::ggplot_build(plot)
  layers <- lapply(seq_along(built$data), function(i)
    plot_layer_signature(built, i, isTRUE(contract$default_style)))
  scales <- c(
    lapply(built$layout$panel_scales_x, scale_signature),
    lapply(built$layout$panel_scales_y, scale_signature)
  )
  aesthetic_scales <- Filter(function(s) !any(c("x", "y") %in% s$aesthetics), built$plot$scales$scales)
  if (length(aesthetic_scales)) {
    names(aesthetic_scales) <- vapply(aesthetic_scales, function(s) s$aesthetics[[1L]], "")
    scales <- c(scales, lapply(aesthetic_scales[order(names(aesthetic_scales))], scale_signature))
  }
  coord <- built$layout$coord
  coordinates <- list(type = class(coord)[1L], ratio = coord$ratio, reverse = coord$reverse,
                      ranges = lapply(built$layout$panel_params, function(p) c(p$x.range, p$y.range)))
  panels <- built$layout$layout
  facets <- which(!names(panels) %in% c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))
  # Facet aliases have no effect on the strip values or panel membership.
  if (length(facets)) {
    panels[facets] <- lapply(panels[facets], as.character)
    names(panels)[facets] <- paste0("facet", seq_along(facets))
  }
  list(layers = layers, panels = list(type = class(built$layout$facet)[1L], data = panels),
       scales = scales, coordinates = coordinates,
       labels = if (length(contract$labels)) ggplot2::get_labs(plot)[contract$labels],
       theme = if (isTRUE(contract$theme)) plot$theme)
}

grade_plot <- function(actual, expected, contract, run_student) {
  stopifnot(inherits(expected, "ggplot"))
  target <- plot_signature(expected, contract)
  checks <- check_result("Plot object", inherits(actual, "ggplot"), "Return a ggplot object.")
  if (!all(checks$passed)) return(checks)
  result <- run_student(plot_signature(actual, contract))
  checks <- rbind(checks,
    check_result("Layer count", length(result$layers) == length(target$layers),
                 paste("Expected", length(target$layers), "layer(s).")),
    check_result("Facets", same_value(result$panels, target$panels), "Check facet variables and panel layout."),
    check_result("Coordinates", same_value(result$coordinates, target$coordinates),
                 "Check the displayed coordinate system, axis directions, and panel limits."),
    check_result("Scales", same_value(result$scales, target$scales),
                 "Check scale limits, transformations, breaks and labels."))
  for (i in seq_len(min(length(result$layers), length(target$layers)))) {
    expected_layer <- target$layers[[i]]
    actual_layer <- result$layers[[i]]
    for (property in c("geom", "stat", "position", "mappings", "aesthetics", "output")) {
      checks <- rbind(checks, check_result(paste("Layer", i, property),
        same_value(actual_layer[[property]], expected_layer[[property]]),
        paste("Check the", property, "of layer", i)))
    }
  }
  if (length(contract$labels))
    checks <- rbind(checks, check_result("Plot labels", same_value(result$labels, target$labels),
                                         "Check the requested plot labels."))
  if (isTRUE(contract$theme))
    checks <- rbind(checks, check_result("Plot theme", same_value(result$theme, target$theme),
                                         "Apply the requested theme."))
  checks
}

grade_submission <- function(spec, submission) {
  suppressPackageStartupMessages(library(tidyverse))
  # Register with.mids before evaluating models on a serialized imputation.
  # Namespaced use leaves tidyr::complete() available under its usual name.
  if (identical(spec$category, "Missing Data")) loadNamespace("mice")
  stopifnot(is.character(spec$solution), length(spec$solution) == 1L,
            is.list(spec$fixtures), length(spec$fixtures) >= 2L, is.list(spec$contract))
  kind <- spec$contract$kind
  stopifnot(length(kind) == 1L, kind %in% c("value", "table", "function", "plot"))
  fields <- switch(kind, value = NULL, table = c("ordered", "column_order", "storage"),
                   `function` = "cases", plot = c("default_style", "theme", "labels"))
  stopifnot(!anyDuplicated(names(spec$contract)), all(names(spec$contract) %in% c("kind", fields)))
  for (flag in intersect(c("ordered", "column_order", "default_style", "theme"), names(spec$contract))) {
    stopifnot(identical(spec$contract[[flag]], TRUE) || identical(spec$contract[[flag]], FALSE))
  }
  if ("labels" %in% names(spec$contract)) stopifnot(is.character(spec$contract$labels))
  reference <- parse(text = spec$solution, keep.source = FALSE)
  stopifnot(length(reference) > 0L)
  warnings <- character()
  finish <- function(status, checks) list(status = status, checks = checks, warnings = unique(warnings))
  parsed <- tryCatch(parse(text = submission, keep.source = FALSE), error = identity)
  if (inherits(parsed, "error")) {
    return(finish("syntax_error", check_result("R syntax", FALSE, conditionMessage(parsed))))
  }
  if (!length(parsed)) return(finish("incorrect", check_result("Answer", FALSE, "Write an R expression to check.")))
  checks <- list()
  # Input objects and bindings are fresh for each fixture and each implementation.
  for (i in seq_along(spec$fixtures)) {
    fixture <- spec$fixtures[[i]]
    stopifnot(is.list(fixture), !anyDuplicated(names(fixture)),
              !length(fixture) || all(nzchar(names(fixture))))
    run <- function(expr) {
      seed <- if (is.null(spec$seed)) 1 else as.double(spec$seed)
      set.seed(as.integer((seed + i) %% .Machine$integer.max))
      eval(expr, list2env(fixture, parent = parent.env(globalenv())))
    }
    origin <- if (i == 1L) "Preview" else paste("Hidden", i - 1L)
    expected <- suppressWarnings(run(reference))
    if (kind == "value" && (is.null(expected) || !(is.atomic(expected) || is.list(expected))))
      stop("A value contract needs a non-NULL reference value.")
    run_student <- function(expr) tryCatch(
      withCallingHandlers(expr, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) stop(errorCondition(conditionMessage(e), class = "submission_error")))
    current <- tryCatch({
      actual <- run_student(run(parsed))
      switch(spec$contract$kind,
        table = grade_table(actual, expected, spec$contract),
        value = check_result("Output value and type",
          identical(column_kind(actual), column_kind(expected)) && same_value(actual, expected),
          "Check the returned value, its type, shape, and names."),
        `function` = grade_function(actual, expected, spec$contract, run_student),
        plot = grade_plot(actual, expected, spec$contract, run_student))
    }, submission_error = identity)
    if (inherits(current, "submission_error")) {
      checks[[i]] <- check_result(paste(origin, "execution"), FALSE, conditionMessage(current))
      return(finish("runtime_error", do.call(rbind, checks)))
    }
    current$label <- paste(origin, current$label, sep = " / ")
    checks[[i]] <- current
  }
  checks <- do.call(rbind, checks)
  stopifnot(nrow(checks) > 0L)
  finish(if (all(checks$passed)) "pass" else "incorrect", checks)
}
