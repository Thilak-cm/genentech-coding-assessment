#' Calculate Interquartile Range (IQR)
#'
#' Calculates the Interquartile Range (IQR = Q3 - Q1) of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the Interquartile Range (IQR).
#' @examples
#' calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' calc_iqr(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))
#' calc_iqr(c(1, NA, 3, 4, 5))
#' @export
calc_iqr <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  if (length(x) == 0) {
    stop("Input vector must not be empty.")
  }
  
  q3 <- quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
  q1 <- quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE)
  
  return(q3 - q1)
}
