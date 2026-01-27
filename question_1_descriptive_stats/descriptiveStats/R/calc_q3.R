#' Calculate third quartile (Q3)
#'
#' Calculates the third quartile (75th percentile) of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the third quartile (Q3).
#' @examples
#' calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' calc_q3(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))
#' calc_q3(c(1, NA, 3, 4, 5))
#' @export
calc_q3 <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  if (length(x) == 0) {
    stop("Input vector must not be empty.")
  }
  
  quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE)
}
