context("test generic digraph flow")

test_that("numbers only digraph", {
  dot <- flow_exclusions_dot(c(1000, 300, 150, 75, 38))
  testthat::expect_error(DiagrammeR::grViz(dot), regexp = NA)
})

test_that("labels and numbers digraph", {
  dot <-
    flow_exclusions_dot(
      incl_counts = c(972, 132, 77, 14),
      total_label = "Total Screened",
      incl_labels = c("Consented", "Completed Study", "BMI <= 30"),
      excl_labels = c("Declined Consent", "Failed to Complete", "BMI > 30")
    )
  testthat::expect_error(DiagrammeR::grViz(dot), regexp = NA)
})

test_that("percents", {
  dot <- flow_exclusions_dot(incl_counts = c(1000, 300, 150),
                            percent_of_total = TRUE)
  testthat::expect_error(DiagrammeR::grViz(dot), regexp = NA)
  dot <- flow_exclusions_dot(incl_counts = c(1000, 300, 150),
                            percent_of_prev = TRUE)
  testthat::expect_error(DiagrammeR::grViz(dot), regexp = NA)
})
