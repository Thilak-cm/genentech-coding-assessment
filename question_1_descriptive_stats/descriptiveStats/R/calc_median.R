#' Calculate median
#'
#' Calculates the median of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the median.
#' @examples
#' calc_median(c(1, 2, 3, 4, 5)) # Median is 3
#' calc_median(c(1, 2, 3, 4)) # Median is 2.5, ie, the average of middle two numbers, 2 and 3.
#' calc_median(c(1, NA, 3, 4, 5)) # Median is 3.5. NA is dropped
#' calc_median(c(5)) # Median is 5 (single value)
#' calc_median(numeric(0)) # Error: Input vector must not be empty.
#' calc_median(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' @export
calc_median <- function(x) {
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
  
  median(x_clean)
}
