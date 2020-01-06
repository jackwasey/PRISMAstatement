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

test_that("dupes box", {
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
           extra_dupes_box = TRUE),
    regexp = NA)
})

test_that("change a label", {
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
           labels = list(screened = "SCREENED")),
    regexp = NA)
})

test_that("use an apostrophe in a label", {
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
           labels = list(screened = "PLEASE DON'T RETURN AN ERROR")),
    regexp = NA)
})

test_that("numbers must work together", {
  expect_error(prisma(1, 2, 3, 4, 5, 6, 7, 8, 9))
  expect_warning(prisma(1000, 20, 270, 270, 10, 260, 19, 240, 107))
  expect_warning(prisma(1000, 20, 270, 270, 269, 260, 20, 240, 107))
})

context("pdf")

test_that("create PDF output", {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE))
    skip("need DiagrammeRsvg and rsvg for this test")
  tmp_pdf <- tempfile()
  on.exit(unlink(tmp_pdf), add = TRUE)
  prsm <- prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107)
  PRISMAstatement:::prisma_pdf(prsm, tmp_pdf)
  expect_true(file.size(tmp_pdf) > 0)
})
