# The drill picker and sequential sessions share this order. Individual drills
# remain available directly, including advanced material.
missing_data_stages <- list(
  "Inspect the records" = c("md_intake", "md_sentinels", "md_field_profile", "md_record_profile",
                            "md_eligible", "md_visits", "md_join_gaps"),
  "Diagnose missingness" = c("md_patterns", "md_coabsence", "md_followup", "md_rate_plot", "md_overlap",
                             "md_mechanism_rules", "md_equivalence"),
  "Recover values and assess exclusions" = c("missing_source_reconcile", "missing_identity",
    "missing_carry", "missing_eligible", "missing_observation_weight"),
  "Build and check imputations" = c("mice_methods", "mice_predictors", "mice_where", "mice_fit",
    "mice_complete", "mice_chains", "mice_distributions", "mice_pool"),
  "Validate predictions" = c("missing_train_only", "missing_mask_validation"),
  "Assess sensitivity and report" = c("missing_binary_bounds", "missing_delta_grid", "missing_compare_reports")
)

ordered_drills <- function(pool) {
  position <- match(pool$id, unlist(missing_data_stages, use.names = FALSE))
  # For other categories (and newly registered drills), keep the usual
  # difficulty/title ordering. Unknown drills come after the authored path.
  pool[order(dplyr::coalesce(position, .Machine$integer.max), pool$difficulty, pool$title), ]
}

drill_choices <- function(pool, category) {
  labels <- setNames(pool$id, pool$title)
  mixed <- setNames(paste0("category/", category), "Mixed drills in this category")
  if (category != "Missing Data") return(c(labels, mixed))
  groups <- lapply(missing_data_stages, function(ids) as.list(labels[labels %in% ids]))
  remaining <- labels[!labels %in% unlist(missing_data_stages, use.names = FALSE)]
  if (length(remaining)) groups[["Additional practice"]] <- as.list(remaining)
  c(Filter(length, groups), as.list(mixed))
}
