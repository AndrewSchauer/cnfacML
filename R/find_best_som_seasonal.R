#' Find Best SOM Model Using Season-Based Cross-Validation
#'
#' Performs multiple iterations of SOM training with a consistent season-based
#' train/test split to find the best performing model. The data split remains
#' constant across iterations, with only SOM initialization varying.
#' Supports flexible training configurations (supervised/unsupervised).
#'
#' @param data_list A list of matrices to be used to train the SOM model. Should
#'   include elements for predictor variables (e.g., numeric, binary) and optionally
#'   danger_prev and danger. The 'danger_prev' element contains the previous day's
#'   danger levels (used as a predictor), while 'danger' contains the current day's
#'   danger levels (the prediction target). The 'danger' element is always required
#'   for evaluation, even if not used in training.
#' @param date_column A vector of dates corresponding to the rows in the data.
#' @param n_iterations Integer. Number of SOM training iterations to perform.
#'   Default is 10.
#' @param n_test_seasons Integer. Number of seasons to use for testing.
#'   Only used if \code{test_seasons} is NULL. Default is 3.
#' @param test_seasons Integer vector. Specific season years to use for testing.
#'   If NULL, seasons are randomly selected. Default is NULL. Using the same
#'   \code{test_seasons} across multiple calls ensures comparable results.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param rlen Integer. Number of training iterations per SOM. Default is 1000.
#' @param alpha Numeric vector of length 2. Learning rate range c(start, end).
#'   Default is c(0.05, 0.01).
#' @param selection_metric Character. Metric to use for selecting the best model.
#'   Options: "weighted_f1" (default) or "mean_accuracy".
#' @param include_danger Logical. If TRUE, includes current danger levels as a
#'   training layer (supervised learning). If FALSE, uses unsupervised clustering
#'   and assigns danger labels post-hoc. Default is TRUE.
#' @param include_danger_prev Logical. If TRUE, includes previous danger levels
#'   as a training layer (temporal predictor). Default is TRUE.
#' @param seed Integer. Random seed for reproducible season selection when
#'   \code{test_seasons} is NULL. Default is 123.
#' @param verbose Logical. If TRUE, prints progress information. Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{best_model}{The complete results object from the best iteration,
#'       as returned by \code{\link{evaluate_som_prediction_presplit}}}
#'     \item{best_iteration}{Integer indicating which iteration produced the best model}
#'     \item{all_results}{List of results from all iterations}
#'     \item{performance_summary}{Data frame with performance metrics for each iteration:
#'       \itemize{
#'         \item iteration: Iteration number
#'         \item alp_accuracy, tl_accuracy, btl_accuracy: Accuracy by elevation
#'         \item alp_weighted_f1, tl_weighted_f1, btl_weighted_f1: F1 scores by elevation
#'         \item mean_accuracy: Average accuracy across elevations
#'         \item mean_weighted_f1: Average F1 score across elevations
#'       }
#'     }
#'     \item{split_info}{Information about the train/test split used}
#'     \item{selection_metric}{Character string indicating which metric was used}
#'     \item{selection_value}{Numeric value of the selection metric for best model}
#'     \item{training_config}{Configuration details including include_danger and include_danger_prev}
#'   }
#'
#' @details
#' This function implements a robust model selection procedure for SOM-based
#' avalanche danger prediction:
#'
#' \strong{Key Features:}
#' \itemize{
#'   \item Maintains a consistent train/test split across all iterations
#'   \item Only SOM initialization varies between iterations
#'   \item Evaluates performance on unseen temporal periods (seasons)
#'   \item Tracks multiple performance metrics
#'   \item Selects best model based on mean weighted F1 or accuracy
#'   \item Supports both supervised and unsupervised learning modes
#' }
#'
#' \strong{Workflow:}
#' \enumerate{
#'   \item Create a season-based data split (once)
#'   \item Train \code{n_iterations} SOM models with different random initializations
#'   \item Evaluate each model on the held-out test seasons
#'   \item Select the model with best performance on the selection metric
#'   \item Return comprehensive results including all iterations
#' }
#'
#' \strong{Model Selection:}
#' \itemize{
#'   \item \code{weighted_f1}: Selects model with highest mean weighted F1 score
#'     across three elevation bands (recommended for imbalanced classes)
#'   \item \code{mean_accuracy}: Selects model with highest mean accuracy
#' }
#'
#' \strong{Training Configurations:}
#' \itemize{
#'   \item \strong{Supervised with temporal info} (include_danger=TRUE, include_danger_prev=TRUE):
#'     4 layers - Uses both current and previous danger in training
#'   \item \strong{Unsupervised with temporal info} (include_danger=FALSE, include_danger_prev=TRUE):
#'     3 layers - Uses only previous danger, predicts current from clusters
#'   \item \strong{Supervised without temporal info} (include_danger=TRUE, include_danger_prev=FALSE):
#'     3 layers - Uses current danger but not previous danger
#'   \item \strong{Purely unsupervised} (include_danger=FALSE, include_danger_prev=FALSE):
#'     2 layers - Pure clustering on numeric and binary features only
#' }
#'
#' \strong{Data Structure:}
#' The \code{data_list} must contain:
#' \itemize{
#'   \item \code{numeric}: Numeric features (required)
#'   \item \code{binary}: Binary features (required)
#'   \item \code{danger}: Current day's danger levels (required for evaluation)
#'   \item \code{danger_prev}: Previous day's danger levels (optional, used if include_danger_prev=TRUE)
#' }
#'
#' @note
#' For comparing different grid sizes or hyperparameters, use the same
#' \code{test_seasons} across all comparisons to ensure fair evaluation.
#'
#' @examples
#' \dontrun{
#' # Prepare data list
#' data_list <- list(
#'   numeric = my_numeric_data,
#'   binary = my_binary_data,
#'   danger_prev = my_danger_prev_data,  # Previous day's danger (predictor)
#'   danger = my_danger_data              # Current day's danger (target)
#' )
#'
#' # Example 1: Supervised with temporal info (default, 4 layers)
#' best_som_supervised <- find_best_som_seasonal(
#'   data_list = data_list,
#'   date_column = my_dates,
#'   n_iterations = 10,
#'   n_test_seasons = 3,
#'   include_danger = TRUE,
#'   include_danger_prev = TRUE
#' )
#'
#' # Example 2: Unsupervised with temporal info (3 layers)
#' best_som_unsupervised <- find_best_som_seasonal(
#'   data_list = data_list,
#'   date_column = my_dates,
#'   n_iterations = 10,
#'   n_test_seasons = 3,
#'   include_danger = FALSE,
#'   include_danger_prev = TRUE
#' )
#'
#' # Example 3: Purely unsupervised (2 layers)
#' best_som_pure <- find_best_som_seasonal(
#'   data_list = data_list,
#'   date_column = my_dates,
#'   n_iterations = 10,
#'   n_test_seasons = 3,
#'   include_danger = FALSE,
#'   include_danger_prev = FALSE
#' )
#'
#' # Access best model and results
#' best_model <- best_som_supervised$best_model$som_model
#' performance <- best_som_supervised$performance_summary
#' plot(performance$mean_weighted_f1, type = "l")
#'
#' # Compare different configurations with same test seasons
#' split <- split_data_by_season(
#'   data_list = data_list,
#'   date_column = dates,
#'   n_test_seasons = 3
#' )
#' test_seasons <- split$test_seasons
#'
#' results_list <- list()
#' configs <- list(
#'   supervised = c(TRUE, TRUE),
#'   unsupervised_temporal = c(FALSE, TRUE),
#'   pure_unsupervised = c(FALSE, FALSE)
#' )
#'
#' for (config_name in names(configs)) {
#'   results_list[[config_name]] <- find_best_som_seasonal(
#'     data_list = data_list,
#'     date_column = dates,
#'     test_seasons = test_seasons,
#'     include_danger = configs[[config_name]][1],
#'     include_danger_prev = configs[[config_name]][2],
#'     n_iterations = 10
#'   )
#' }
#' }
#'
#' @seealso
#' \code{\link{split_data_by_season}}, \code{\link{evaluate_som_prediction_presplit}}
#'
#' @export
find_best_som_seasonal <- function(data_list,
                                   date_column,
                                   n_iterations = 10,
                                   n_test_seasons = 3,
                                   test_seasons = NULL,
                                   xdim = 15, ydim = 11,
                                   rlen = 1000, alpha = c(0.05, 0.01),
                                   selection_metric = "weighted_f1",
                                   include_danger = TRUE,
                                   include_danger_prev = TRUE,
                                   seed = 123,
                                   verbose = TRUE) {

  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("FINDING BEST SOM MODEL ACROSS", n_iterations, "ITERATIONS\n")
  cat("(Season-Based Split)\n")

  # Describe training configuration
  if (include_danger && include_danger_prev) {
    cat("Training mode: Supervised with temporal info (4 layers)\n")
  } else if (!include_danger && include_danger_prev) {
    cat("Training mode: Unsupervised with temporal info (3 layers)\n")
  } else if (include_danger && !include_danger_prev) {
    cat("Training mode: Supervised without temporal info (3 layers)\n")
  } else {
    cat("Training mode: Purely unsupervised (2 layers)\n")
  }

  cat("=" , rep("=", 70), "\n", sep = "")

  # Validate data_list structure
  if (!"danger" %in% names(data_list)) {
    stop("data_list must contain a 'danger' element (required for evaluation)")
  }

  # Check for danger_prev if needed
  if (include_danger_prev && !"danger_prev" %in% names(data_list)) {
    stop("include_danger_prev=TRUE but 'danger_prev' not found in data_list")
  }

  if (verbose) {
    cat("\nData layers in data_list:\n")
    for (name in names(data_list)) {
      if (name == "danger") {
        if (include_danger) {
          cat("  -", name, "(prediction target - used in training)\n")
        } else {
          cat("  -", name, "(prediction target - NOT used in training, only for evaluation)\n")
        }
      } else if (name == "danger_prev") {
        if (include_danger_prev) {
          cat("  -", name, "(predictor: previous day's danger - used in training)\n")
        } else {
          cat("  -", name, "(available but NOT used in training)\n")
        }
      } else {
        cat("  -", name, "(predictor)\n")
      }
    }
  }

  # Create the split ONCE - will be consistent across all iterations
  split <- split_data_by_season(data_list = data_list,
                                date_column = date_column,
                                n_test_seasons = n_test_seasons,
                                test_seasons = test_seasons,
                                seed = seed)

  cat("\nSelection metric:", selection_metric, "\n")
  cat("Grid size:", xdim, "x", ydim, "\n")
  cat("Training configuration:\n")
  cat("  - Include danger in training:", include_danger, "\n")
  cat("  - Include danger_prev in training:", include_danger_prev, "\n")
  cat("\nNote: Same train/test split used for all iterations\n")
  cat("      (only SOM initialization varies)\n\n")

  # Store results from all iterations
  all_results <- list()
  performance_summary <- data.frame(
    iteration = integer(),
    alp_accuracy = numeric(),
    tl_accuracy = numeric(),
    btl_accuracy = numeric(),
    alp_weighted_f1 = numeric(),
    tl_weighted_f1 = numeric(),
    btl_weighted_f1 = numeric(),
    mean_accuracy = numeric(),
    mean_weighted_f1 = numeric()
  )

  # Run multiple iterations with same data split
  for (i in 1:n_iterations) {

    if (verbose) {
      cat("Running iteration", i, "of", n_iterations, "...\n")
    }

    # Run evaluation with consistent split and training configuration
    results <- evaluate_som_prediction_presplit(
      split_data = split,
      xdim = xdim,
      ydim = ydim,
      rlen = rlen,
      alpha = alpha,
      include_danger = include_danger,
      include_danger_prev = include_danger_prev
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

  # Select best model
  if (selection_metric == "weighted_f1") {
    best_idx <- which.max(performance_summary$mean_weighted_f1)
    metric_value <- performance_summary$mean_weighted_f1[best_idx]
  } else if (selection_metric == "mean_accuracy") {
    best_idx <- which.max(performance_summary$mean_accuracy)
    metric_value <- performance_summary$mean_accuracy[best_idx]
  } else {
    stop("selection_metric must be either 'weighted_f1' or 'mean_accuracy'")
  }

  best_results <- all_results[[best_idx]]

  # Print summary
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("BEST MODEL SELECTION COMPLETE\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nBest model: Iteration", best_idx, "\n")
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

  return(list(
    best_model = best_results,
    best_iteration = best_idx,
    all_results = all_results,
    performance_summary = performance_summary,
    split_info = split,
    selection_metric = selection_metric,
    selection_value = metric_value,
    training_config = list(
      include_danger = include_danger,
      include_danger_prev = include_danger_prev
    )
  ))
}
