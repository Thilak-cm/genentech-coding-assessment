test_that("calc_median calculates median correctly", {
  # Odd number of elements
  expect_equal(calc_median(c(1, 2, 3, 4, 5)), 3)
  expect_equal(calc_median(c(10, 20, 30, 40, 50)), 30)
  
  # Even number of elements (average of middle two)
  expect_equal(calc_median(c(1, 2, 3, 4)), 2.5)
  expect_equal(calc_median(c(10, 20, 30, 40)), 25)
  
  # Single value
  expect_equal(calc_median(c(5)), 5)
  expect_equal(calc_median(c(0)), 0)
  
  # Two values
  expect_equal(calc_median(c(1, 3)), 2)
})

test_that("calc_median handles NA values correctly", {
  # NA values are removed
  expect_equal(calc_median(c(1, NA, 3, 4, 5)), 3.5)
  expect_equal(calc_median(c(1, 2, NA, 4)), 2.5)
  expect_equal(calc_median(c(NA, 2, 3, NA)), 2.5)
})

test_that("calc_median handles edge cases and errors", {
  # Empty vector
  expect_error(calc_median(numeric(0)), "Input vector must not be empty")
  
  # All NA values
  expect_error(calc_median(c(NA, NA, NA)), "Input vector contains only missing values")
  
  # Non-numeric input
  expect_error(calc_median(c("a", "b", "c")), "Input must be a numeric vector")
  expect_error(calc_median(c(TRUE, FALSE)), "Input must be a numeric vector")
})

test_that("calc_median works with example from assessment", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_median(data), 4.5)
})
