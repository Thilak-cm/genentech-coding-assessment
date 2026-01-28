#' Calculate arithmetic mean
#'
#' Calculates the arithmetic mean of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the mean.
#' @examples
#' calc_mean(c(1, 2, 3)) # Mean is 2
#' calc_mean(c(1, NA, 3)) # Mean is 2. NA is dropped here
#' calc_mean(c(5)) # Mean is 5 (single value)
#' \donttest{
#' calc_mean(numeric(0)) # Error: Input vector must not be empty.
#' calc_mean(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' }
#' @export
calc_mean <- function(x) {
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
  
  mean(x_clean)
}
