#' Evaluate Ensemble SOM Predictions
#'
#' Evaluates the performance of ensemble predictions from multiple SOM models
#' by creating confusion matrices and calculating performance metrics for all
#' three elevation bands.
#'
#' @param ensemble_results A results object from \code{\link{ensemble_predict_som}}
#'   containing \code{ensemble_predictions} and \code{mean_agreement} elements.
#' @param actual_danger A data frame with actual danger levels containing columns:
#'   \code{alp.used}, \code{tl.used}, \code{btl.used}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{confusion_matrices}{List with confusion matrices for alp, tl, btl}
#'     \item{metrics}{List with performance metrics for alp, tl, btl, each containing:
#'       \itemize{
#'         \item \code{confusion_matrix}: The confusion matrix
#'         \item \code{accuracy}: Overall accuracy
#'         \item \code{metrics}: Data frame with precision, recall, F1 per class
#'       }
#'     }
#'   }
#'
#' @details
#' This function provides comprehensive evaluation of ensemble predictions:
#'
#' \strong{Evaluation Steps:}
#' \enumerate{
#'   \item Create confusion matrices for each elevation band
#'   \item Calculate performance metrics (precision, recall, F1, accuracy)
#'   \item Print detailed results including model agreement
#' }
#'
#' \strong{Printed Output:}
#' For each elevation band (ALP, TL, BTL), the function prints:
#' \itemize{
#'   \item Confusion matrix
#'   \item Per-class metrics (precision, recall, F1, support)
#'   \item Overall accuracy
#'   \item Mean model agreement (from ensemble)
#' }
#'
#' The model agreement indicates the average percentage of models that agreed
#' with the final ensemble prediction, providing insight into prediction confidence.
#'
#' @note
#' This function requires \code{\link{create_confusion_matrix}} and
#' \code{\link{calculate_metrics}} to be available.
#'
#' @examples
#' \dontrun{
#' # After creating ensemble predictions
#' ensemble <- ensemble_predict_som(som_models, test_data)
#'
#' # Evaluate ensemble performance
#' eval_results <- evaluate_ensemble(ensemble, actual_danger = test_data$danger)
#'
#' # Access confusion matrices
#' print(eval_results$confusion_matrices$alp)
#'
#' # Access metrics
#' alp_accuracy <- eval_results$metrics$alp$accuracy
#' alp_f1 <- eval_results$metrics$alp$metrics$F1
#'
#' # Compare with individual model performance
#' cat("Ensemble accuracy:", alp_accuracy, "\n")
#' }
#'
#' @seealso
#' \code{\link{ensemble_predict_som}}, \code{\link{create_confusion_matrix}},
#' \code{\link{calculate_metrics}}, \code{\link{plot_ensemble_distribution}}
#'
#' @export
evaluate_ensemble <- function(ensemble_results, actual_danger) {
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("ENSEMBLE PREDICTION EVALUATION\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  
  predictions <- ensemble_results$ensemble_predictions
  
  # Create confusion matrices
  cm_alp <- create_confusion_matrix(actual_danger$alp.used, predictions$predicted_alp)
  cm_tl <- create_confusion_matrix(actual_danger$tl.used, predictions$predicted_tl)
  cm_btl <- create_confusion_matrix(actual_danger$btl.used, predictions$predicted_btl)
  
  # Calculate metrics
  metrics_alp <- calculate_metrics(cm_alp)
  metrics_tl <- calculate_metrics(cm_tl)
  metrics_btl <- calculate_metrics(cm_btl)
  
  # Print results
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("RESULTS: ALP.USED\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nConfusion Matrix:\n")
  print(cm_alp)
  cat("\nPerformance Metrics:\n")
  print(metrics_alp$metrics, row.names = FALSE)
  cat("\nOverall Accuracy:", round(metrics_alp$accuracy, 4), "\n")
  cat("Mean Model Agreement:", round(ensemble_results$mean_agreement$alp * 100, 1), "%\n")
  
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("RESULTS: TL.USED\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nConfusion Matrix:\n")
  print(cm_tl)
  cat("\nPerformance Metrics:\n")
  print(metrics_tl$metrics, row.names = FALSE)
  cat("\nOverall Accuracy:", round(metrics_tl$accuracy, 4), "\n")
  cat("Mean Model Agreement:", round(ensemble_results$mean_agreement$tl * 100, 1), "%\n")
  
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("RESULTS: BTL.USED\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nConfusion Matrix:\n")
  print(cm_btl)
  cat("\nPerformance Metrics:\n")
  print(metrics_btl$metrics, row.names = FALSE)
  cat("\nOverall Accuracy:", round(metrics_btl$accuracy, 4), "\n")
  cat("Mean Model Agreement:", round(ensemble_results$mean_agreement$btl * 100, 1), "%\n")
  
  return(list(
    confusion_matrices = list(alp = cm_alp, tl = cm_tl, btl = cm_btl),
    metrics = list(alp = metrics_alp, tl = metrics_tl, btl = metrics_btl)
  ))
}
