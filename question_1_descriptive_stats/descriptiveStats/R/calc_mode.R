#' Calculate mode
#'
#' Calculates the mode (most frequently occurring value) of a numeric vector.
#' Handles ties by returning the first mode encountered.
#' Returns NA if no mode exists (all values are unique).
#' Missing values are removed by default.
#'
#' @param x A numeric vector.
#' @return A numeric scalar representing the mode, or NA if no mode exists.
#' @examples
#' calc_mode(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))  # Returns 5
#' calc_mode(c(1, 2, 3, 4, 5))  # Returns NA (no mode)
#' calc_mode(c(1, 2, 2, 3, 3))  # Returns 2 (first mode in case of tie)
#' calc_mode(c(1, NA, 2, 2, 3))  # Returns 2. NA is dropped
#' calc_mode(c(5))  # Returns NA (single value, no mode)
#' calc_mode(numeric(0)) # Error: Input vector must not be empty.
#' calc_mode(c(NA, NA, NA)) # Error: Input vector contains only missing values.
#' @export
calc_mode <- function(x) {
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
  
  # Count frequency of each value
  freq_table <- table(x_clean)
  max_freq <- max(freq_table)
  
  # If all values have frequency 1, no mode exists
  if (max_freq == 1) {
    return(NA_real_)
  }
  
  # Get the mode(s) - values with maximum frequency
  modes <- as.numeric(names(freq_table)[freq_table == max_freq])
  
  # Return the first mode (handles ties by returning first encountered)
  return(modes[1])
}
