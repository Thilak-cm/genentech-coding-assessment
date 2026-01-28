test_that("calc_mode calculates mode correctly", {
  # Single mode
  expect_equal(calc_mode(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)), 5)
  expect_equal(calc_mode(c(1, 1, 1, 2, 3)), 1)
  expect_equal(calc_mode(c(1, 2, 3, 3, 3)), 3)
  
  # Multiple modes (returns first encountered)
  expect_equal(calc_mode(c(1, 2, 2, 3, 3)), 2)
  expect_equal(calc_mode(c(1, 1, 2, 2, 3)), 1)
})

test_that("calc_mode returns NA when no mode exists", {
  # All unique values
  expect_true(is.na(calc_mode(c(1, 2, 3, 4, 5))))
  expect_true(is.na(calc_mode(c(10, 20, 30))))
  
  # Single value (no mode)
  expect_true(is.na(calc_mode(c(5))))
  expect_true(is.na(calc_mode(c(0))))
})

test_that("calc_mode handles NA values correctly", {
  # NA values are removed
  expect_equal(calc_mode(c(1, NA, 2, 2, 3)), 2)
  expect_equal(calc_mode(c(NA, 1, 1, 2, NA)), 1)
  expect_equal(calc_mode(c(1, 2, 2, NA, 3, 3)), 2)  # First mode
})

test_that("calc_mode handles edge cases and errors", {
  # Empty vector
  expect_error(calc_mode(numeric(0)), "Input vector must not be empty")
  
  # All NA values
  expect_error(calc_mode(c(NA, NA, NA)), "Input vector contains only missing values")
  
  # Non-numeric input
  expect_error(calc_mode(c("a", "b", "c")), "Input must be a numeric vector")
  expect_error(calc_mode(c(TRUE, FALSE)), "Input must be a numeric vector")
})

test_that("calc_mode works with example from assessment", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_mode(data), 5)
})
