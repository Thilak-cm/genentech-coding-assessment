test_that("calc_iqr calculates interquartile range correctly", {
  # Standard cases
  expect_equal(calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), 4.5)
  expect_equal(calc_iqr(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)), 3)
  
  # Single value (IQR = 0)
  expect_equal(calc_iqr(c(5)), 0)
  
  # Two values
  q1 <- quantile(c(1, 5), 0.25, names = FALSE)
  q3 <- quantile(c(1, 5), 0.75, names = FALSE)
  expect_equal(calc_iqr(c(1, 5)), q3 - q1)
})

test_that("calc_iqr handles NA values correctly", {
  # NA values are removed
  expect_equal(calc_iqr(c(1, NA, 3, 4, 5)), 
               quantile(c(1, 3, 4, 5), 0.75, names = FALSE) - 
               quantile(c(1, 3, 4, 5), 0.25, names = FALSE))
  expect_equal(calc_iqr(c(NA, 2, 3, NA, 5)), 
               quantile(c(2, 3, 5), 0.75, names = FALSE) - 
               quantile(c(2, 3, 5), 0.25, names = FALSE))
})

test_that("calc_iqr handles edge cases and errors", {
  # Empty vector
  expect_error(calc_iqr(numeric(0)), "Input vector must not be empty")
  
  # All NA values
  expect_error(calc_iqr(c(NA, NA, NA)), "Input vector contains only missing values")
  
  # Non-numeric input
  expect_error(calc_iqr(c("a", "b", "c")), "Input must be a numeric vector")
  expect_error(calc_iqr(c(TRUE, FALSE)), "Input must be a numeric vector")
})

test_that("calc_iqr works with example from assessment", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_iqr(data), 3)
})

test_that("calc_iqr relationship with Q1 and Q3", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  q1_val <- calc_q1(data)
  q3_val <- calc_q3(data)
  iqr_val <- calc_iqr(data)
  
  # IQR should equal Q3 - Q1
  expect_equal(iqr_val, q3_val - q1_val)
})
