test_that("calc_q3 calculates third quartile correctly", {
  # Standard cases
  expect_equal(calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), 7.75)
  expect_equal(calc_q3(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)), 5.75)
  
  # Single value
  expect_equal(calc_q3(c(5)), 5)
  
  # Two values
  expect_equal(calc_q3(c(1, 5)), quantile(c(1, 5), 0.75, names = FALSE))
})

test_that("calc_q3 handles NA values correctly", {
  # NA values are removed
  expect_equal(calc_q3(c(1, NA, 3, 4, 5)), quantile(c(1, 3, 4, 5), 0.75, names = FALSE))
  expect_equal(calc_q3(c(NA, 2, 3, NA, 5)), quantile(c(2, 3, 5), 0.75, names = FALSE))
})

test_that("calc_q3 handles edge cases and errors", {
  # Empty vector
  expect_error(calc_q3(numeric(0)), "Input vector must not be empty")
  
  # All NA values
  expect_error(calc_q3(c(NA, NA, NA)), "Input vector contains only missing values")
  
  # Non-numeric input
  expect_error(calc_q3(c("a", "b", "c")), "Input must be a numeric vector")
  expect_error(calc_q3(c(TRUE, FALSE)), "Input must be a numeric vector")
})

test_that("calc_q3 works with example from assessment", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_q3(data), 5.5)
})
