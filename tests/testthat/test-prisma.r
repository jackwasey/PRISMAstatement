context("basic")

test_that("prisma runs without error", {
  expect_error(
    prisma(found = 750,
           found_other = 123,
           no_dupes = 776,
           screened = 776,
           screen_exclusions = 13,
           full_text = 763,
           full_text_exclusions = 17,
           qualitative = 746,
           quantitative = 319),
    regexp = NA)
})

test_that("prisma passes values to grViz", {
  expect_error(
    prisma(found = 750,
           found_other = 123,
           no_dupes = 776,
           screened = 776,
           screen_exclusions = 13,
           full_text = 763,
           full_text_exclusions = 17,
           qualitative = 746,
           quantitative = 319,
           width = 800, height = 800),
    regexp = NA)
})

test_that("all values could be zero", {
  expect_error(
    prisma(found = 0,
           found_other = 0,
           no_dupes = 0,
           screened = 0,
           screen_exclusions = 0,
           full_text = 0,
           full_text_exclusions = 0,
           qualitative = 0,
           quantitative = 0,
           width = 800, height = 800),
    regexp = NA)
})

test_that("can vary DPI", {
  expect_error(
    prisma(found = 0,
           found_other = 0,
           no_dupes = 0,
           screened = 0,
           screen_exclusions = 0,
           full_text = 0,
           full_text_exclusions = 0,
           qualitative = 0,
           quantitative = 0,
           width = 800, height = 800,
           dpi = 300),
    regexp = NA)
})

# someday: later numbers must be lower or equal to earlier ones in the sequence.
