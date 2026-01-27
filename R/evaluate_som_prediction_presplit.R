#' Evaluate SOM Prediction Performance with Pre-Split Data
#'
#' Trains a Self-Organizing Map (SOM) model on training data and evaluates its
#' predictive performance on test data using pre-split datasets. Designed for
#' avalanche danger level prediction with three elevation bands.
#' Supports flexible layer configurations.
#'
#' @param split_data A list containing pre-split data, typically the output from
#'   \code{\link{split_data_by_season}}. Must contain:
#'   \itemize{
#'     \item \code{train}: List with numeric, binary, and optionally danger/danger_prev data
#'     \item \code{test}: List with numeric, binary, and optionally danger/danger_prev data
#'     \item \code{train_indices}: Integer vector of training row indices
#'     \item \code{test_indices}: Integer vector of test row indices
#'     \item \code{split_method}: Character string describing split method
#'     \item \code{test_seasons}: (Optional) Vector of test season years
#'   }
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param rlen Integer. Number of training iterations for the SOM. Default is 1000.
#' @param alpha Numeric vector of length 2. Learning rate range c(start, end).
#'   Default is c(0.05, 0.01).
#' @param include_danger Logical. If TRUE, includes current danger levels as a
#'   training layer (supervised learning). Default is TRUE.
#' @param include_danger_prev Logical. If TRUE, includes previous danger levels
#'   as a training layer (predictor). Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{som_model}{The trained supersom model object}
#'     \item{predictions}{List with predicted_alp, predicted_tl, predicted_btl}
#'     \item{confusion_matrices}{List with confusion matrices for alp, tl, btl}
#'     \item{metrics}{List with performance metrics for alp, tl, btl, each containing:
#'       \itemize{
#'         \item \code{confusion_matrix}: The confusion matrix
#'         \item \code{accuracy}: Overall accuracy
#'         \item \code{metrics}: Data frame with precision, recall, F1 per class
#'       }
#'     }
#'     \item{split_info}{Information about the train/test split used}
#'     \item{training_config}{Configuration details including include_danger,
#'       include_danger_prev, and n_layers}
#'   }
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Validates and prepares the pre-split data
#'   \item Trains a supersom model with flexible layer configuration:
#'     \itemize{
#'       \item Layer 1: Numeric features (sum of squares distance) - ALWAYS included
#'       \item Layer 2: Binary features (Tanimoto distance) - ALWAYS included
#'       \item Layer 3 (optional): Current danger levels (sum of squares distance) - if include_danger=TRUE
#'       \item Layer 4 (optional): Previous danger levels (sum of squares distance) - if include_danger_prev=TRUE
#'     }
#'   \item Maps test data using appropriate layers (excluding current danger)
#'   \item Makes predictions from node assignments
#'   \item Creates confusion matrices for three elevation bands:
#'     \itemize{
#'       \item ALP (Alpine)
#'       \item TL (Treeline)
#'       \item BTL (Below Treeline)
#'     }
#'   \item Calculates performance metrics (accuracy, precision, recall, F1)
#' }
#'
#' The function prints detailed progress information and a summary of results
#' to the console.
#'
#' @section Layer Configurations:
#' You can train the SOM with different layer combinations:
#' \itemize{
#'   \item \strong{Supervised with temporal info} (include_danger=TRUE, include_danger_prev=TRUE):
#'     4 layers - Uses both current and previous danger in training
#'   \item \strong{Unsupervised with temporal info} (include_danger=FALSE, include_danger_prev=TRUE):
#'     3 layers - Uses only previous danger, current danger predicted from clustering
#'   \item \strong{Supervised without temporal info} (include_danger=TRUE, include_danger_prev=FALSE):
#'     3 layers - Uses current danger but not previous danger
#'   \item \strong{Purely unsupervised} (include_danger=FALSE, include_danger_prev=FALSE):
#'     2 layers - Pure clustering based only on numeric and binary features
#' }
#'
#' @note
#' This function requires the following functions to be available:
#' \itemize{
#'   \item \code{supersom()} from the kohonen package
#'   \item \code{predict_danger_from_som()}
#'   \item \code{create_confusion_matrix()}
#'   \item \code{calculate_metrics()}
#' }
#'
#' @examples
#' \dontrun{
#' # First, create a season-based split
#' data_list <- list(
#'   numeric = my_numeric_data,
#'   binary = my_binary_data,
#'   danger = my_current_danger,      # Current day (target)
#'   danger_prev = my_previous_danger # Previous day (predictor)
#' )
#'
#' split <- split_data_by_season(
#'   data_list = data_list,
#'   date_column = dates,
#'   n_test_seasons = 3
#' )
#'
#' # Default: Supervised with temporal info (4 layers)
#' results_supervised <- evaluate_som_prediction_presplit(
#'   split_data = split,
#'   xdim = 15,
#'   ydim = 11,
#'   rlen = 1000
#' )
#'
#' # Unsupervised with temporal info (3 layers)
#' results_unsupervised <- evaluate_som_prediction_presplit(
#'   split_data = split,
#'   xdim = 15,
#'   ydim = 11,
#'   rlen = 1000,
#'   include_danger = FALSE,
#'   include_danger_prev = TRUE
#' )
#'
#' # Purely unsupervised (2 layers)
#' results_pure <- evaluate_som_prediction_presplit(
#'   split_data = split,
#'   xdim = 15,
#'   ydim = 11,
#'   rlen = 1000,
#'   include_danger = FALSE,
#'   include_danger_prev = FALSE
#' )
#'
#' # Access results
#' model <- results_supervised$som_model
#' accuracy_alp <- results_supervised$metrics$alp$accuracy
#' f1_scores <- results_supervised$metrics$alp$metrics$F1
#' }
#'
#' @seealso
#' \code{\link{split_data_by_season}}, \code{\link{find_best_som_seasonal}}
#'
#' @importFrom kohonen supersom somgrid
#' @export
evaluate_som_prediction_presplit <- function(split_data,
                                             xdim = 15, ydim = 11,
                                             rlen = 1000,
                                             alpha = c(0.05, 0.01),
                                             include_danger = TRUE,
                                             include_danger_prev = TRUE) {

  cat("\nStarting SOM Predictive Performance Evaluation\n")

  # Dynamic messaging based on configuration
  n_layers <- 2  # Always have numeric + binary
  layer_desc <- "numeric, binary"

  if (include_danger) {
    n_layers <- n_layers + 1
    layer_desc <- paste0(layer_desc, ", danger")
  }

  if (include_danger_prev) {
    n_layers <- n_layers + 1
    layer_desc <- paste0(layer_desc, ", danger_prev")
  }

  cat("Training:", n_layers, "layers (", layer_desc, ")\n", sep = "")
  cat("=" , rep("=", 50), "\n", sep = "")

  cat("\n1. Using pre-split data...\n")
  cat("   Training observations:", length(split_data$train_indices), "\n")
  cat("   Test observations:", length(split_data$test_indices), "\n")
  cat("   Split method:", split_data$split_method, "\n")
  if (!is.null(split_data$test_seasons)) {
    cat("   Test seasons:", paste(split_data$test_seasons, collapse = ", "), "\n")
  }

  # 2. Build training data dynamically
  cat("\n2. Training SOM model...\n")
  cat("   Layer 1: Numeric features\n")
  cat("   Layer 2: Binary features\n")

  train_list <- list(
    numeric = as.matrix(split_data$train$numeric),
    binary = as.matrix(split_data$train$binary)
  )

  dist_fcts <- c("sumofsquares", "tanimoto")
  layer_num <- 3

  # Add danger if requested
  if (include_danger) {
    cat("   Layer", layer_num, ": Current danger (target - used in training)\n")
    train_list$danger <- as.matrix(split_data$train$danger)
    dist_fcts <- c(dist_fcts, "sumofsquares")
    layer_num <- layer_num + 1
  }

  # Add danger_prev if requested
  if (include_danger_prev) {
    cat("   Layer", layer_num, ": Previous danger (predictor)\n")
    train_list$danger_prev <- as.matrix(split_data$train$danger_prev)
    dist_fcts <- c(dist_fcts, "sumofsquares")
  }

  # Validate configuration
  if (!include_danger && !include_danger_prev) {
    cat("   Note: Training without danger or danger_prev - purely unsupervised clustering\n")
  } else if (!include_danger) {
    cat("   Note: Training without current danger - unsupervised with temporal info\n")
  }

  som_model <- kohonen::supersom(
    data = train_list,
    grid = kohonen::somgrid(xdim = xdim, ydim = ydim, topo = 'hexagonal'),
    rlen = rlen,
    alpha = alpha,
    dist.fcts = dist_fcts,
    keep.data = TRUE
  )
  if (!include_danger) {
    # Store danger information for prediction later
    som_model$danger_labels <- split_data$train$danger
  }
  cat("   Model trained successfully!\n")

  # 3. Predict on test data
  cat("\n3. Making predictions on test data...\n")

  # Build test data to match training configuration (excluding current danger)
  test_layers <- c("numeric", "binary")
  if (include_danger_prev) {
    test_layers <- c(test_layers, "danger_prev")
  }
  cat("   Mapping with layers:", paste(test_layers, collapse = ", "), "\n")
  cat("   Predicting current danger from node assignments\n")

  predictions <- predict_danger_from_som(
    som_model,
    split_data$test,
    include_danger = include_danger,
    include_danger_prev = include_danger_prev
  )

  # Actual values are current danger (the target)
  actual_alp <- split_data$test$danger$alp.used
  actual_tl <- split_data$test$danger$tl.used
  actual_btl <- split_data$test$danger$btl.used

  # 4. Create confusion matrices
  cat("\n4. Creating confusion matrices...\n")
  cm_alp <- create_confusion_matrix(actual_alp, predictions$predicted_alp, levels = sort(unique(predictions$predicted_alp)))
  cm_tl <- create_confusion_matrix(actual_tl, predictions$predicted_tl, levels = sort(unique(predictions$predicted_tl)))
  cm_btl <- create_confusion_matrix(actual_btl, predictions$predicted_btl, levels = sort(unique(predictions$predicted_btl)))

  # 5. Calculate metrics
  cat("\n5. Calculating performance metrics...\n")
  metrics_alp <- calculate_metrics(cm_alp)
  metrics_tl <- calculate_metrics(cm_tl)
  metrics_btl <- calculate_metrics(cm_btl)

  # Print summary
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("PERFORMANCE SUMMARY\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nALP.USED  - Accuracy:", round(metrics_alp$accuracy, 4),
      "| Weighted F1:", round(metrics_alp$metrics$F1[nrow(metrics_alp$metrics)], 4), "\n")
  cat("TL.USED   - Accuracy:", round(metrics_tl$accuracy, 4),
      "| Weighted F1:", round(metrics_tl$metrics$F1[nrow(metrics_tl$metrics)], 4), "\n")
  cat("BTL.USED  - Accuracy:", round(metrics_btl$accuracy, 4),
      "| Weighted F1:", round(metrics_btl$metrics$F1[nrow(metrics_btl$metrics)], 4), "\n")

  mean_acc <- mean(c(metrics_alp$accuracy, metrics_tl$accuracy, metrics_btl$accuracy))
  mean_f1 <- mean(c(
    metrics_alp$metrics$F1[nrow(metrics_alp$metrics)],
    metrics_tl$metrics$F1[nrow(metrics_tl$metrics)],
    metrics_btl$metrics$F1[nrow(metrics_btl$metrics)]
  ))
  cat("MEAN      - Accuracy:", round(mean_acc, 4),
      "| Weighted F1:", round(mean_f1, 4), "\n")

  # Return results
  return(list(
    som_model = som_model,
    predictions = predictions,
    confusion_matrices = list(
      alp = cm_alp,
      tl = cm_tl,
      btl = cm_btl
    ),
    metrics = list(
      alp = metrics_alp,
      tl = metrics_tl,
      btl = metrics_btl
    ),
    split_info = list(
      split_method = split_data$split_method,
      n_train = length(split_data$train_indices),
      n_test = length(split_data$test_indices),
      test_seasons = split_data$test_seasons
    ),
    training_config = list(
      include_danger = include_danger,
      include_danger_prev = include_danger_prev,
      n_layers = n_layers
    )
  ))
}
