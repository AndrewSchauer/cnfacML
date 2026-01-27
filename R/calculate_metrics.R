#' Calculate Classification Metrics from Confusion Matrix
#'
#' Computes precision, recall, F1 score, and overall accuracy for each class
#' from a confusion matrix. Also calculates weighted averages across all classes.
#'
#' @param confusion_matrix A confusion matrix (table object) with rows representing
#'   actual classes and columns representing predicted classes, typically created
#'   by \code{\link{create_confusion_matrix}}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{confusion_matrix}{The input confusion matrix (for reference)}
#'     \item{accuracy}{Overall accuracy (proportion of correct predictions)}
#'     \item{metrics}{Data frame with per-class metrics containing:
#'       \itemize{
#'         \item \code{Class}: Class label
#'         \item \code{Precision}: TP / (TP + FP)
#'         \item \code{Recall}: TP / (TP + FN)
#'         \item \code{F1}: Harmonic mean of precision and recall
#'         \item \code{Support}: Number of actual observations in this class
#'       }
#'       The last row contains weighted averages across all classes.
#'     }
#'   }
#'
#' @details
#' \strong{Metrics Definitions:}
#' \itemize{
#'   \item \strong{Precision}: Of all predictions for class i, what proportion
#'     were correct? (Focuses on false positives)
#'   \item \strong{Recall}: Of all actual class i observations, what proportion
#'     were correctly identified? (Focuses on false negatives)
#'   \item \strong{F1 Score}: Harmonic mean of precision and recall, balances both metrics
#'   \item \strong{Support}: Number of observations actually belonging to each class
#' }
#'
#' \strong{Weighted Averages:}
#' The final row of the metrics data frame contains weighted averages, where
#' each class's metric is weighted by its support (sample size). This accounts
#' for class imbalance.
#'
#' \strong{Handling Edge Cases:}
#' \itemize{
#'   \item If precision or recall cannot be calculated (division by zero),
#'     they are set to 0
#'   \item If both precision and recall are 0, F1 is set to 0
#' }
#'
#' @examples
#' \dontrun{
#' # Create confusion matrix
#' actual <- c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5)
#' predicted <- c(1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5)
#' cm <- create_confusion_matrix(actual, predicted)
#'
#' # Calculate metrics
#' metrics <- calculate_metrics(cm)
#'
#' # Overall accuracy
#' print(paste("Accuracy:", round(metrics$accuracy, 3)))
#'
#' # View per-class metrics
#' print(metrics$metrics)
#'
#' # Get weighted F1 score
#' weighted_f1 <- metrics$metrics$F1[nrow(metrics$metrics)]
#' print(paste("Weighted F1:", round(weighted_f1, 3)))
#' }
#'
#' @seealso
#' \code{\link{create_confusion_matrix}}
#'
#' @export
calculate_metrics <- function(confusion_matrix) {
  n_classes <- nrow(confusion_matrix)
  class_names <- rownames(confusion_matrix)
  
  metrics <- data.frame(
    Class = class_names,
    Precision = numeric(n_classes),
    Recall = numeric(n_classes),
    F1 = numeric(n_classes),
    Support = numeric(n_classes)
  )
  
  for (i in 1:n_classes) {
    # True Positives
    tp <- confusion_matrix[i, i]
    
    # False Positives (predicted as class i but actually other classes)
    fp <- sum(confusion_matrix[, i]) - tp
    
    # False Negatives (actually class i but predicted as other classes)
    fn <- sum(confusion_matrix[i, ]) - tp
    
    # Support (actual number of cases in this class)
    support <- sum(confusion_matrix[i, ])
    
    # Calculate metrics
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    f1 <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
    
    metrics$Precision[i] <- precision
    metrics$Recall[i] <- recall
    metrics$F1[i] <- f1
    metrics$Support[i] <- support
  }
  
  # Calculate overall accuracy
  total_correct <- sum(diag(confusion_matrix))
  total_observations <- sum(confusion_matrix)
  accuracy <- total_correct / total_observations
  
  # Calculate weighted averages
  total_support <- sum(metrics$Support)
  weighted_precision <- sum(metrics$Precision * metrics$Support) / total_support
  weighted_recall <- sum(metrics$Recall * metrics$Support) / total_support
  weighted_f1 <- sum(metrics$F1 * metrics$Support) / total_support
  
  # Add weighted average row
  metrics <- rbind(metrics, data.frame(
    Class = "Weighted Avg",
    Precision = weighted_precision,
    Recall = weighted_recall,
    F1 = weighted_f1,
    Support = total_support
  ))
  
  return(list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    metrics = metrics
  ))
}
