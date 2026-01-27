#' Calculate median
#'
#' Calculates the median of a numeric vector.
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the median.
#' @examples
#' calc_median(c(1, 2, 3, 4, 5))
#' calc_median(c(1, 2, 3, 4))
#' calc_median(c(1, NA, 3, 4, 5))
#' @export
calc_median <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  if (length(x) == 0) {
    stop("Input vector must not be empty.")
  }
  
  median(x, na.rm = TRUE)
}
