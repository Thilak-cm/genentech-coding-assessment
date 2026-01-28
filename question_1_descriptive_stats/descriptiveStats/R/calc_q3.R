#' Calculate third quartile (Q3)
#'
#' Calculates the third quartile (75th percentile) of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the third quartile (Q3).
#' @examples
#' calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) # Q3 is 7.75
#' calc_q3(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)) # Q3 is 5.75
#' calc_q3(c(1, NA, 3, 4, 5)) # Q3 is 4.5
#' calc_q3(c(5)) # Q3 is 5 (single value)
#' \donttest{
#' calc_q3(numeric(0)) # Error: Input vector must not be empty.
#' calc_q3(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' }
#' @export
calc_q3 <- function(x) {
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
  
  quantile(x_clean, probs = 0.75, na.rm = FALSE, names = FALSE)
}
