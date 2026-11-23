scheduler_history <- function(day, seed = seq_along(day), hints = 0L, status = "pass") {
  stamp <- as.numeric(as.POSIXct(paste(as.Date("2025-03-01") + day, "12:00:00"), tz = ""))
  data.frame(id = seq_along(day), template_id = "grouped_summary", seed = seed, level = 1L,
             practice_date = as.character(as.Date("2025-03-01") + day), attempted_at = stamp,
             hints = hints, status = status)
}

testthat::test_that("successful review intervals grow only on due dates", {
  daily <- scheduler_history(c(rep(0, 16), 1, 2, 3, 4, 11))
  check <- function(n) schedule_skill(daily[seq_len(n), ])
  testthat::expect_identical(check(16)$level, "Developing")
  testthat::expect_equal(check(16)$retention_days, 0)
  testthat::expect_equal(check(16)$due_at, next_review_midnight("2025-03-01", 1))
  testthat::expect_identical(check(17)$level, "Proficient")
  testthat::expect_equal(check(17)$due_at, next_review_midnight("2025-03-02", 3))
  testthat::expect_equal(check(19)$due_at, check(17)$due_at)
  testthat::expect_equal(check(20)$retention_days, 2L)
  testthat::expect_equal(check(21)$retention_days, 3L)
  testthat::expect_identical(check(21)$level, "Automatic")
})

testthat::test_that("mistakes, hints, repeat fixtures and infrastructure errors get distinct treatment", {
  a <- scheduler_history(c(0, 0, 0, 0, 0, 1), c(1, 1, 2, 3, 3, 4),
                         c(0, 0, 1, 0, 0, 0),
                         c("incorrect", "pass", "pass", "infrastructure_error", "pass", "pass"))
  first <- schedule_skill(a[1, ])
  testthat::expect_equal(first$due_at, a$attempted_at[1] + 600)
  retry <- schedule_skill(a[1:2, ])
  testthat::expect_equal(retry$due_at, first$due_at)
  testthat::expect_equal(retry$unseen_successes, 0L)
  score <- schedule_skill(a)
  testthat::expect_equal(score$unseen_successes, 2L)
  testthat::expect_equal(score$attempts, 5L)
  testthat::expect_equal(score$retention_days, 1L)
  testthat::expect_null(schedule_skill(a[4, ]))
  repeats <- scheduler_history(rep(0, 20), seed = 1)
  repeats$level <- rep(c(1L, 2L), 10L)
  testthat::expect_identical(schedule_skill(repeats)$level, "Beginner")
  testthat::expect_equal(schedule_skill(repeats)$streak, 1L)
})

testthat::test_that("frequent mistakes sort ahead of another equally overdue skill", {
  weak <- scheduler_history(rep(0, 5), status = "incorrect")
  strong <- scheduler_history(0L)
  now <- weak$attempted_at[1L] + 172800
  testthat::expect_gt(schedule_skill(weak, now)$priority, schedule_skill(strong, now)$priority)
})

testthat::test_that("guided practice earns completion with no independent or delayed-review credit", {
  h <- scheduler_history(c(0, 0, 1, 1, 1), seed = c(1L, 2L, 3L, 3L, 4L))
  h$guide_shown <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  score <- function(rows) schedule_skill(h[rows, , drop = FALSE])
  first <- score(1L)
  testthat::expect_equal(first$correct, 1L)
  testthat::expect_equal(first$unseen_successes, 0L)
  testthat::expect_equal(first$due_at, h$attempted_at[1L] + 600)
  due <- score(1:2)
  testthat::expect_equal(due$streak, 1L)
  guide_on_due_day <- score(1:3)
  testthat::expect_equal(guide_on_due_day$due_at, due$due_at)
  testthat::expect_equal(guide_on_due_day$retention_days, 0L)
  testthat::expect_equal(guide_on_due_day$streak, 0L)
  replay <- score(1:4)
  testthat::expect_equal(replay$unseen_successes, 1L)
  testthat::expect_equal(replay$retention_days, 0L)
  recovered <- score(1:5)
  testthat::expect_equal(recovered$retention_days, 1L)
  testthat::expect_equal(recovered$unseen_successes, 2L)
  testthat::expect_equal(recovered$due_at, next_review_midnight(h$practice_date[5L], 3L))
})

testthat::test_that("daily streaks allow practice through yesterday", {
  days <- as.Date("2025-01-01") + c(0, 0, 1, 5, 6, 7)
  testthat::expect_equal(practice_streaks(days, max(days)), c(current = 3L, longest = 3L))
  testthat::expect_equal(practice_streaks(days, max(days) + 1), c(current = 3L, longest = 3L))
  testthat::expect_equal(practice_streaks(days, max(days) + 2), c(current = 0L, longest = 3L))
  testthat::expect_equal(practice_streaks(character()), c(current = 0L, longest = 0L))
})
