#' Get Mode with Maximum Value Tie-Breaking
#'
#' Finds the most frequent value in a vector. When multiple values have the same
#' maximum frequency (ties), returns the highest value among them. This is useful
#' for danger level prediction where ties should be resolved conservatively by
#' selecting the higher danger rating.
#'
#' @param x A numeric or integer vector.
#'
#' @return A single numeric value representing the mode. If there are multiple
#'   modes (values with equal maximum frequency), returns the highest value
#'   among them.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Counts the frequency of each unique value in \code{x}
#'   \item Identifies the maximum frequency
#'   \item Finds all values that have this maximum frequency (the modes)
#'   \item Returns the highest value among the modes
#' }
#'
#' This tie-breaking strategy is particularly important for avalanche danger
#' prediction, as selecting the higher danger level in ambiguous cases is the
#' conservative and safer choice.
#'
#' @examples
#' # Single mode - most frequent value
#' get_mode_max_tie(c(1, 2, 2, 3, 3, 3, 4))
#' # Returns: 3
#'
#' # Tie case - two values equally frequent
#' get_mode_max_tie(c(1, 1, 2, 2, 3))
#' # Returns: 2 (higher of the two modes)
#'
#' # All equal frequency - returns maximum
#' get_mode_max_tie(c(1, 2, 3, 4, 5))
#' # Returns: 5
#'
#' # Danger level example with tie
#' danger_levels <- c(2, 2, 3, 3, 4)
#' get_mode_max_tie(danger_levels)
#' # Returns: 3 (conservative choice)
#'
#' @export
get_mode_max_tie <- function(x) {
  # Count frequencies
  freq_table <- table(x)
  
  # Find maximum frequency
  max_freq <- max(freq_table)
  
  # Get all values with maximum frequency
  modes <- as.numeric(names(freq_table[freq_table == max_freq]))
  
  # Return the highest value among the modes
  return(max(modes))
}
