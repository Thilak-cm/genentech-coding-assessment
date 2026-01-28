#' Calculate Interquartile Range (IQR)
#'
#' Calculates the Interquartile Range (IQR = Q3 - Q1) of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the Interquartile Range (IQR).
#' @examples
#' calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) # IQR is 4.5
#' calc_iqr(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)) # IQR is 3
#' calc_iqr(c(1, NA, 3, 4, 5)) # IQR is 2.5
#' calc_iqr(c(5)) # IQR is 0 (single value)
#' calc_iqr(numeric(0)) # Error: Input vector must not be empty.
#' calc_iqr(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' @export
calc_iqr <- function(x) {
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
  
  q3 <- quantile(x_clean, probs = 0.75, na.rm = FALSE, names = FALSE)
  q1 <- quantile(x_clean, probs = 0.25, na.rm = FALSE, names = FALSE)
  
  return(q3 - q1)
}
