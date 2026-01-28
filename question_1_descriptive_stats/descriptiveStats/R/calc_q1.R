#' Calculate first quartile (Q1)
#'
#' Calculates the first quartile (25th percentile) of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the first quartile (Q1).
#' @examples
#' calc_q1(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) # Q1 is 3.25
#' calc_q1(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)) # Q1 is 2.25
#' calc_q1(c(1, NA, 3, 4, 5)) # Q1 is 2.5
#' calc_q1(c(5)) # Q1 is 5 (single value)
#' \donttest{
#' calc_q1(numeric(0)) # Error: Input vector must not be empty.
#' calc_q1(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' }
#' @export
calc_q1 <- function(x) {
  if (length(x) == 0) {
    stop("Input vector must not be empty.")
  }
  
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  # Remove missing values
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) == 0) {
    stop("Input vector contains only missing values.")
  }
  
  quantile(x_clean, probs = 0.25, na.rm = FALSE, names = FALSE)
}
