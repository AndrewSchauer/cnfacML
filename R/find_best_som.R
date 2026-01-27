#' Find Best SOM Model Using Multiple Random Initializations
#'
#' Performs multiple iterations of SOM training with different random seeds and
#' selects the best performing model. Uses random or sequential data splitting.
#' Employs 4-layer training and 3-layer testing structure.
#'
#' @param dataNumeric A data frame or matrix containing numeric predictor variables.
#' @param dataBinary A data frame or matrix containing binary predictor variables.
#' @param dataDanger A data frame or matrix containing CURRENT day's danger level 
#'   outcomes. Used in training and as prediction target.
#' @param dataDangerPrev A data frame or matrix containing PREVIOUS day's danger 
#'   level outcomes. Used as a predictor in both training and testing.
#' @param n_iterations Integer. Number of SOM training iterations to perform.
#'   Default is 10.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param test_size Numeric between 0 and 1. Proportion of data for testing.
#'   Default is 0.2.
#' @param rlen Integer. Number of training iterations per SOM. Default is 1000.
#' @param alpha Numeric vector of length 2. Learning rate range c(start, end).
#'   Default is c(0.05, 0.01).
#' @param selection_metric Character. Metric to use for selecting best model.
#'   Options: "weighted_f1" (default), "mean_accuracy", "alp_accuracy",
#'   "tl_accuracy", "btl_accuracy".
#' @param seed_start Integer. Starting seed for iterations. Each iteration uses
#'   seed_start + i - 1. Default is 123.
#' @param split_method Character. Data splitting method: "random" (default) or
#'   "sequential"/"temporal".
#' @param verbose Logical. If TRUE, prints progress information. Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{best_model}{Complete results from best iteration (from
#'       \code{\link{evaluate_som_prediction}})}
#'     \item{best_iteration}{Integer indicating which iteration produced best model}
#'     \item{best_seed}{Random seed used for the best model}
#'     \item{all_results}{List of results from all iterations}
#'     \item{performance_summary}{Data frame with metrics for each iteration:
#'       \itemize{
#'         \item iteration, seed: Iteration number and random seed
#'         \item alp_accuracy, tl_accuracy, btl_accuracy: Accuracy by elevation
#'         \item alp_weighted_f1, tl_weighted_f1, btl_weighted_f1: F1 by elevation
#'         \item mean_accuracy, mean_weighted_f1: Averages across elevations
#'       }
#'     }
#'     \item{selection_metric}{Character string indicating which metric was used}
#'     \item{selection_value}{Numeric value of the selection metric for best model}
#'   }
#'
#' @details
#' This function implements model selection through multiple random initializations:
#'
#' \strong{SOM Structure:}
#' \itemize{
#'   \item TRAINING: 4 layers (numeric, binary, danger, danger_prev)
#'   \item TESTING: 3 layers (numeric, binary, danger_prev)
#'   \item TARGET: Predict current danger from previous danger + features
#' }
#'
#' \strong{Key Differences from find_best_som_seasonal:}
#' \itemize{
#'   \item Uses random or sequential splitting (not season-based)
#'   \item Different random seed AND data split for each iteration
#'   \item Suitable when temporal structure is not critical
#' }
#'
#' \strong{Iteration Process:}
#' \enumerate{
#'   \item For each iteration i:
#'     \itemize{
#'       \item Use seed = seed_start + i - 1
#'       \item Split data (different split each iteration if random)
#'       \item Train SOM with 4 layers using random initialization
#'       \item Test using 3 layers (without current danger)
#'       \item Evaluate predictions against current danger
#'     }
#'   \item Select iteration with best performance on selection_metric
#'   \item Return best model and all results
#' }
#'
#' \strong{Selection Metrics:}
#' \itemize{
#'   \item \code{weighted_f1}: Mean weighted F1 across three elevations (default)
#'   \item \code{mean_accuracy}: Mean accuracy across three elevations
#'   \item \code{alp_accuracy}: Alpine accuracy only
#'   \item \code{tl_accuracy}: Treeline accuracy only
#'   \item \code{btl_accuracy}: Below treeline accuracy only
#' }
#'
#' The function prints comprehensive statistics including performance ranges
#' and standard deviations across all iterations.
#'
#' @note
#' For time-series data, consider using \code{\link{find_best_som_seasonal}} with
#' season-based splitting for more robust temporal validation.
#'
#' @examples
#' \dontrun{
#' # Find best model with default settings
#' best_som <- find_best_som(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,          # Current day
#'   dataDangerPrev = my_danger_prev_data, # Previous day
#'   n_iterations = 10
#' )
#'
#' # Sequential split for temporal data
#' best_som <- find_best_som(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,
#'   dataDangerPrev = my_danger_prev_data,
#'   n_iterations = 10,
#'   split_method = "sequential"
#' )
#'
#' # Select based on alpine accuracy only
#' best_som <- find_best_som(
#'   dataNumeric, dataBinary, dataDanger, dataDangerPrev,
#'   selection_metric = "alp_accuracy"
#' )
#'
#' # Access results
#' best_model <- best_som$best_model$som_model
#' performance <- best_som$performance_summary
#' cat("Best seed:", best_som$best_seed, "\n")
#'
#' # Plot performance across iterations
#' plot(performance$mean_weighted_f1, type = "b",
#'      main = "F1 Score Across Iterations")
#' }
#'
#' @seealso
#' \code{\link{evaluate_som_prediction}}, \code{\link{find_best_som_seasonal}},
#' \code{\link{plot_iteration_performance}}
#'
#' @export
find_best_som <- function(dataNumeric, dataBinary, dataDanger, dataDangerPrev,
                          n_iterations = 10,
                          xdim = 15, ydim = 11,
                          test_size = 0.2,
                          rlen = 1000, alpha = c(0.05, 0.01),
                          selection_metric = "weighted_f1",
                          seed_start = 123,
                          split_method = "random",
                          verbose = TRUE) {
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("FINDING BEST SOM MODEL ACROSS", n_iterations, "ITERATIONS\n")
  cat("Training: 4 layers (numeric, binary, danger, danger_prev)\n")
  cat("Testing: 3 layers (numeric, binary, danger_prev)\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nSelection metric:", selection_metric, "\n")
  cat("Split method:", split_method, "\n")
  cat("Grid size:", xdim, "x", ydim, "\n")
  cat("Test size:", test_size * 100, "%\n\n")
  
  # Store results from all iterations
  all_results <- list()
  performance_summary <- data.frame(
    iteration = integer(),
    seed = integer(),
    alp_accuracy = numeric(),
    tl_accuracy = numeric(),
    btl_accuracy = numeric(),
    alp_weighted_f1 = numeric(),
    tl_weighted_f1 = numeric(),
    btl_weighted_f1 = numeric(),
    mean_accuracy = numeric(),
    mean_weighted_f1 = numeric()
  )
  
  # Run multiple iterations
  for (i in 1:n_iterations) {
    current_seed <- seed_start + i - 1
    
    if (verbose) {
      cat("Running iteration", i, "of", n_iterations, "(seed =", current_seed, ")...\n")
    }
    
    # Evaluate SOM with current seed
    results <- evaluate_som_prediction(
      dataNumeric = dataNumeric,
      dataBinary = dataBinary,
      dataDanger = dataDanger,
      dataDangerPrev = dataDangerPrev,
      xdim = xdim,
      ydim = ydim,
      test_size = test_size,
      seed = current_seed,
      rlen = rlen,
      alpha = alpha,
      split_method = split_method
    )
    
    # Store results
    all_results[[i]] <- results
    
    # Extract performance metrics
    alp_acc <- results$metrics$alp$accuracy
    tl_acc <- results$metrics$tl$accuracy
    btl_acc <- results$metrics$btl$accuracy
    
    alp_wf1 <- results$metrics$alp$metrics$F1[nrow(results$metrics$alp$metrics)]
    tl_wf1 <- results$metrics$tl$metrics$F1[nrow(results$metrics$tl$metrics)]
    btl_wf1 <- results$metrics$btl$metrics$F1[nrow(results$metrics$btl$metrics)]
    
    mean_acc <- mean(c(alp_acc, tl_acc, btl_acc))
    mean_wf1 <- mean(c(alp_wf1, tl_wf1, btl_wf1))
    
    performance_summary <- rbind(performance_summary, data.frame(
      iteration = i,
      seed = current_seed,
      alp_accuracy = alp_acc,
      tl_accuracy = tl_acc,
      btl_accuracy = btl_acc,
      alp_weighted_f1 = alp_wf1,
      tl_weighted_f1 = tl_wf1,
      btl_weighted_f1 = btl_wf1,
      mean_accuracy = mean_acc,
      mean_weighted_f1 = mean_wf1
    ))
    
    if (verbose) {
      cat("  Mean Accuracy:", round(mean_acc, 4),
          "| Mean Weighted F1:", round(mean_wf1, 4), "\n")
    }
  }
  
  # Select best model based on specified metric
  if (selection_metric == "weighted_f1") {
    best_idx <- which.max(performance_summary$mean_weighted_f1)
    metric_value <- performance_summary$mean_weighted_f1[best_idx]
  } else if (selection_metric == "mean_accuracy") {
    best_idx <- which.max(performance_summary$mean_accuracy)
    metric_value <- performance_summary$mean_accuracy[best_idx]
  } else if (selection_metric == "alp_accuracy") {
    best_idx <- which.max(performance_summary$alp_accuracy)
    metric_value <- performance_summary$alp_accuracy[best_idx]
  } else if (selection_metric == "tl_accuracy") {
    best_idx <- which.max(performance_summary$tl_accuracy)
    metric_value <- performance_summary$tl_accuracy[best_idx]
  } else if (selection_metric == "btl_accuracy") {
    best_idx <- which.max(performance_summary$btl_accuracy)
    metric_value <- performance_summary$btl_accuracy[best_idx]
  } else {
    stop("Invalid selection_metric. Choose from: weighted_f1, mean_accuracy, alp_accuracy, tl_accuracy, btl_accuracy")
  }
  
  best_results <- all_results[[best_idx]]
  best_seed <- performance_summary$seed[best_idx]
  
  # Print summary statistics
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("BEST MODEL SELECTION COMPLETE\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nBest model: Iteration", best_idx, "(seed =", best_seed, ")\n")
  cat("Selection metric (", selection_metric, "):", round(metric_value, 4), "\n\n")
  
  cat("Performance of best model:\n")
  cat("  ALP.USED    - Accuracy:", round(performance_summary$alp_accuracy[best_idx], 4),
      "| Weighted F1:", round(performance_summary$alp_weighted_f1[best_idx], 4), "\n")
  cat("  TL.USED     - Accuracy:", round(performance_summary$tl_accuracy[best_idx], 4),
      "| Weighted F1:", round(performance_summary$tl_weighted_f1[best_idx], 4), "\n")
  cat("  BTL.USED    - Accuracy:", round(performance_summary$btl_accuracy[best_idx], 4),
      "| Weighted F1:", round(performance_summary$btl_weighted_f1[best_idx], 4), "\n")
  cat("  MEAN        - Accuracy:", round(performance_summary$mean_accuracy[best_idx], 4),
      "| Weighted F1:", round(performance_summary$mean_weighted_f1[best_idx], 4), "\n")
  
  cat("\nPerformance across all iterations:\n")
  cat("  Mean Accuracy       - Range: [", round(min(performance_summary$mean_accuracy), 4), 
      ", ", round(max(performance_summary$mean_accuracy), 4), "]",
      " | SD:", round(sd(performance_summary$mean_accuracy), 4), "\n", sep = "")
  cat("  Mean Weighted F1    - Range: [", round(min(performance_summary$mean_weighted_f1), 4), 
      ", ", round(max(performance_summary$mean_weighted_f1), 4), "]",
      " | SD:", round(sd(performance_summary$mean_weighted_f1), 4), "\n", sep = "")
  
  return(list(
    best_model = best_results,
    best_iteration = best_idx,
    best_seed = best_seed,
    all_results = all_results,
    performance_summary = performance_summary,
    selection_metric = selection_metric,
    selection_value = metric_value
  ))
}
