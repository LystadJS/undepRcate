# Check an author-specified fixture against a hand-calculated result through
# the real worker. The library-wide tests also grade every generated level.
expect_inspection_table <- function(id, level, inputs, expected, seed = 735L) {
  ex <- make_exercise(id, seed, level)
  ex$fixtures <- rep(list(inputs), 3L)
  code <- paste(deparse(expected), collapse = "\n")
  graded <- assess_answer(ex, code)
  testthat::expect_equal(graded$status, "pass", info = paste(id, paste(graded$checks$detail, collapse = " / ")))
}

testthat::test_that("intake preserves literal tokens, quoting, source and field types", {
  csv <- paste("person,visit,reading,note", '0007,02,,"review, later"', "0021,03,MISS,NA",
               "0041,01,0,MISS", "0062,04,8.25,OK", sep = "\n")
  raw <- data.frame(person = c("0007", "0021", "0041", "0062"), visit = c("02", "03", "01", "04"),
                    reading = c("", "MISS", "0", "8.25"), note = c("review, later", "NA", "MISS", "OK"))
  expect_inspection_table("md_intake", 1, list(csv = csv), raw)
  parsed <- transform(raw, visit = c(2L, 3L, 1L, 4L), reading = c(NA_real_, NA_real_, 0, 8.25))
  expect_inspection_table("md_intake", 3, list(feeds = list(alpha = csv, beta = csv)),
                          rbind(transform(parsed, source = "alpha"), transform(parsed, source = "beta")))
  for (level in 2:3) {
    ex <- make_exercise("md_intake", 13L, level)
    wrong <- sub("readr::parse_integer(visit)", "readr::parse_double(visit)", ex$solution, fixed = TRUE)
    testthat::expect_identical(assess_answer(ex, wrong)$status, "incorrect")
  }
})

testthat::test_that("reasons, structural eligibility and response indicators survive cleaning", {
  seed <- 243L
  sampled <- make_exercise("md_sentinels", seed, 3L)$inputs$df$raw
  refused <- intersect(sampled, c(-91L, -97L, -991L))
  skipped <- intersect(sampled, c(-88L, -98L, -998L))
  original <- data.frame(id = 1:5, raw = c(0, -1, refused, skipped, NA_real_))
  correct <- transform(original, clean = c(0, -1, NA, NA, NA),
                       reason = c("observed", "observed", "refused", "not applicable", "unrecorded"),
                       eligible = c(TRUE, TRUE, TRUE, FALSE, TRUE),
                       observed = c(1L, 1L, 0L, NA_integer_, 0L))
  expect_inspection_table("md_sentinels", 3, list(df = original), correct, seed)
  ex <- make_exercise("md_sentinels", seed, 3L)
  double_indicator <- paste0(ex$solution, " |>\n  mutate(observed = as.double(observed))")
  testthat::expect_identical(assess_answer(ex, double_indicator)$status, "incorrect")
})

testthat::test_that("row and field audits exclude metadata and separate partial from complete records", {
  df <- data.frame(record_id = 4:6, site = "North", height = c(NA, 0, NA), weight = c(NA, 1, 7), note = NA_character_)
  inputs <- list(df = df, fields = c("weight", "height"))
  expect_inspection_table("md_field_profile", 3, inputs,
                          data.frame(site = "North", field = c("height", "weight"),
                                     missing = c(2L, 1L), records = 3L, rate = c(2/3, 1/3)))
  expect_inspection_table("md_record_profile", 3, inputs,
                          data.frame(record_id = 4:6, missing = c(2, 0, 1), rate = c(1, 0, .5),
                                     status = c("empty", "complete", "partial"),
                                     absent_fields = c("height, weight", "", "height")))
})

testthat::test_that("unknown eligibility and a zero risk set do not create a response rate", {
  df <- data.frame(site = c("A", "A", "A", "A", "B", "B"), field = "weight",
                   due = c(TRUE, TRUE, FALSE, NA, NA, FALSE), value = c(0, NA, 14, NA, 0, NA))
  correct <- data.frame(site = c("A", "B"), field = "weight", eligible = c(2, 0), missing = c(1, 0),
                        unknown_due = 1L, structural = 1L, unexpected = c(1, 0), rate = c(.5, NA_real_))
  expect_inspection_table("md_eligible", 3, list(df = df), correct)
})

testthat::test_that("pattern digits denote observation and joint and conditional denominators differ", {
  seed <- 510L
  fields <- make_exercise("md_patterns", seed, 3)$inputs$fields
  df <- data.frame(site = c("Bay", "Bay", "Bay", "Hill"), a = c(NA, NA, 0, 0),
                   b = c(NA, 5, 0, NA), c = c(0, 0, 0, NA))
  names(df)[2:4] <- fields
  expect_inspection_table("md_patterns", 3L, list(df = df),
                          data.frame(site = df$site, pattern = c("001", "011", "111", "100"),
                                     records = 1L, share = c(1/3, 1/3, 1/3, 1)), seed)
  expect_inspection_table("md_coabsence", 3L, list(df = df),
                          data.frame(site = c("Bay", "Hill"), both = c(1, 0), first_absent = c(2, 0),
                                     conditional_rate = c(.5, NA_real_), joint_rate = c(1/3, 0)), seed)
})

testthat::test_that("heterogeneous schedules, duplicate visits and incomplete linkage keys are audited", {
  schedule <- data.frame(person = c(1, 1, 2), week = c(0, 2, 6), site = "Hill")
  visits <- data.frame(person = c(1, 1, 2, 2), week = c(0, 0, 2, 6), reading = c(NA, 5, 7, NA))
  expect_inspection_table("md_visits", 3L, list(schedule = schedule, visits = visits),
                          data.frame(site = "Hill", planned = 3, absent = 1, unread = 1, unavailable_rate = 2/3))
  records <- data.frame(row_id = 1:5, person = c(2, 2, 7, NA, 2), site = c("A", "B", "A", "A", NA))
  roster <- data.frame(person = c(2, 2, NA), site = c("A", "B", "A"), value = c(0, NA, 7))
  expect_inspection_table("md_join_gaps", 3L, list(records = records, roster = roster),
                          data.frame(row_id = 1:5, status = c("recorded", "missing value", "unmatched",
                                                              "incomplete key", "incomplete key")))
})

testthat::test_that("classification uses a supplied mechanism and identifies effects within strata", {
  rules <- expand.grid(score = 0:1, stratum = c("East", "West"),
                       protocol = c("fair", "context", "residual"), stringsAsFactors = FALSE)
  # The last rule swaps score effects by stratum; averaging can hide that dependence.
  rules$miss_prob <- c(rep(.5, 4), .2, .2, .8, .8, .2, .8, .8, .2)
  expect_inspection_table("md_mechanism_rules", 3L, list(rules = rules),
                          data.frame(protocol = c("fair", "context", "residual"),
                                     constant = c(TRUE, FALSE, FALSE), residual_dependence = c(FALSE, FALSE, TRUE),
                                     mechanism = c("MCAR", "MAR", "MNAR")))
})

testthat::test_that("different complete means can produce the same observed outcomes and response rate", {
  cells <- expand.grid(outcome = 0:1, recorded = c(FALSE, TRUE), scenario = c("left", "right"), stringsAsFactors = FALSE)
  cells$freq <- c(3L, 3L, 3L, 3L, 1L, 5L, 3L, 3L)
  expect_inspection_table("md_equivalence", 3L, list(cells = cells),
                          data.frame(scenario = c("left", "right"), response_rate = .5, observed_mean = .5,
                                     full_mean = c(.5, 2/3), gap = c(0, 1/6)))
})

testthat::test_that("rate plots reject pooling across fields and using ineligible rows", {
  plot <- make_exercise("md_rate_plot", 99L, 3L)
  testthat::expect_identical(assess_answer(plot, plot$solution)$status, "pass")
  pooled <- sub(" + facet_wrap(vars(field))", "", plot$solution, fixed = TRUE)
  testthat::expect_identical(assess_answer(plot, pooled)$status, "incorrect")
  ineligible <- sub("filter(due %in% TRUE)", "filter(TRUE)", plot$solution, fixed = TRUE)
  testthat::expect_identical(assess_answer(plot, ineligible)$status, "incorrect")
  interior <- make_exercise("md_rate_plot", 3L, 3L)
  auto_limits <- sub(" + scale_y_continuous(limits = c(0, 1))", "", interior$solution, fixed = TRUE)
  testthat::expect_identical(assess_answer(interior, auto_limits)$status, "incorrect")
})
