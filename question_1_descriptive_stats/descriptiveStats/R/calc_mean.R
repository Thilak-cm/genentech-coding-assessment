#' Calculate arithmetic mean
#'
#' Calculates the arithmetic mean of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the mean.
#' @examples
#' calc_mean(c(1, 2, 3))
#' calc_mean(c(1, NA, 3))
#' @export
calc_mean <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  if (length(x) == 0) {
    stop("Input vector must not be empty.")
  }
  
  mean(x, na.rm = TRUE)
}
