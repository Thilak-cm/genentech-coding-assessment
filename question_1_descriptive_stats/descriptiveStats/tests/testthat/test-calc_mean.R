test_that("calc_mean calculates mean correctly", {
  # Basic functionality
  expect_equal(calc_mean(c(1, 2, 3)), 2)
  expect_equal(calc_mean(c(1, 2, 3, 4, 5)), 3)
  expect_equal(calc_mean(c(10, 20, 30)), 20)
  
  # Single value
  expect_equal(calc_mean(c(5)), 5)
  expect_equal(calc_mean(c(0)), 0)
  
  # Decimal values
  expect_equal(calc_mean(c(1.5, 2.5, 3.5)), 2.5)
})

test_that("calc_mean handles NA values correctly", {
  # NA values are removed
  expect_equal(calc_mean(c(1, NA, 3)), 2)
  expect_equal(calc_mean(c(1, 2, NA, 4, 5)), 3)
  expect_equal(calc_mean(c(NA, 2, 3, NA, 5)), 10/3)
})

test_that("calc_mean handles edge cases and errors", {
  # Empty vector
  expect_error(calc_mean(numeric(0)), "Input vector must not be empty")
  
  # All NA values (numeric)
  expect_error(calc_mean(rep(NA_real_, 3)), "Input vector contains only missing values")
  expect_error(calc_mean(as.numeric(c(NA, NA, NA))), "Input vector contains only missing values")
  
  # Non-numeric input
  expect_error(calc_mean(c("a", "b", "c")), "Input must be a numeric vector")
  expect_error(calc_mean(c(TRUE, FALSE)), "Input must be a numeric vector")
  expect_error(calc_mean(list(1, 2, 3)), "Input must be a numeric vector")
})

test_that("calc_mean works with example from assessment", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  # Note: PDF shows 3.3 but actual calculation is 4.3 (43/10)
  # Using actual calculated value
  expect_equal(calc_mean(data), 4.3)
})
