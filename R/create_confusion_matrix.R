#' Create Confusion Matrix for Danger Level Predictions
#'
#' Generates a confusion matrix comparing actual and predicted danger levels
#' for avalanche forecasting. Ensures both actual and predicted values are
#' treated as factors with consistent levels (1-5).
#'
#' @param actual A numeric vector of actual danger levels.
#' @param predicted A numeric vector of predicted danger levels.
#' @param levels A numeric vector specifying the possible danger levels.
#'   Default is 1:5 (standard avalanche danger scale).
#'
#' @return A table object representing the confusion matrix. Rows represent
#'   actual values, columns represent predicted values. Row and column names
#'   are labeled as "Actual" and "Predicted" respectively.
#'
#' @details
#' The confusion matrix is a key tool for evaluating classification performance.
#' Each cell (i, j) contains the count of observations that were actually class i
#' but predicted as class j.
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item Diagonal elements: Correct predictions
#'   \item Off-diagonal elements: Misclassifications
#'   \item Row sums: Total observations per actual class (support)
#'   \item Column sums: Total predictions per class
#' }
#'
#' The function converts both input vectors to factors with identical levels,
#' ensuring that all danger levels appear in the matrix even if they have zero
#' counts. This is important for consistent metric calculation.
#'
#' @examples
#' # Perfect predictions
#' actual <- c(1, 2, 3, 4, 5)
#' predicted <- c(1, 2, 3, 4, 5)
#' cm <- create_confusion_matrix(actual, predicted)
#' print(cm)
#' #        Predicted
#' # Actual  1 2 3 4 5
#' #      1  1 0 0 0 0
#' #      2  0 1 0 0 0
#' #      3  0 0 1 0 0
#' #      4  0 0 0 1 0
#' #      5  0 0 0 0 1
#'
#' # With misclassifications
#' actual <- c(2, 2, 3, 3, 3, 4, 4, 5)
#' predicted <- c(2, 3, 2, 3, 3, 4, 5, 5)
#' cm <- create_confusion_matrix(actual, predicted)
#' print(cm)
#'
#' # Custom levels (if using different scale)
#' cm <- create_confusion_matrix(actual, predicted, levels = 1:3)
#'
#' @seealso
#' \code{\link{calculate_metrics}} for computing precision, recall, and F1 from
#' confusion matrix
#'
#' @export
create_confusion_matrix <- function(actual, predicted, levels = 1:5) {
  # Ensure both are factors with same levels
  actual <- factor(actual, levels = levels)
  predicted <- factor(predicted, levels = levels)

  # Create confusion matrix
  cm <- table(Actual = actual, Predicted = predicted)

  return(cm)
}
