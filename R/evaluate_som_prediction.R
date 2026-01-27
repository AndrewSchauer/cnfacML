#' Evaluate SOM Prediction Performance with Random/Sequential Split
#'
#' Trains a Self-Organizing Map (SOM) model and evaluates its predictive
#' performance using random or sequential data splitting. Provides comprehensive
#' performance metrics for avalanche danger prediction across three elevation bands.
#' Uses 4-layer training and 3-layer testing structure.
#'
#' @param dataNumeric A data frame or matrix containing numeric predictor variables.
#' @param dataBinary A data frame or matrix containing binary predictor variables.
#' @param dataDanger A data frame or matrix containing CURRENT DAY'S danger level 
#'   outcomes (must include columns: alp.used, tl.used, btl.used). This is used
#'   during training and as the prediction target for evaluation.
#' @param dataDangerPrev A data frame or matrix containing PREVIOUS DAY'S danger 
#'   level outcomes (must include columns: alp.used, tl.used, btl.used). This is
#'   used as a predictor variable in both training and testing.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param test_size Numeric between 0 and 1. Proportion of data to use for testing.
#'   Default is 0.2 (20% test, 80% train).
#' @param seed Integer. Random seed for reproducible splits when using random method.
#'   Default is 123.
#' @param rlen Integer. Number of training iterations for the SOM. Default is 1000.
#' @param alpha Numeric vector of length 2. Learning rate range c(start, end).
#'   Default is c(0.05, 0.01).
#' @param split_method Character. Method for splitting data. Options:
#'   \code{"random"} (default) or \code{"sequential"}/\code{"temporal"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{som_model}{The trained supersom model object}
#'     \item{split}{List containing train/test data and indices from \code{\link{split_data}}}
#'     \item{predictions}{Data frame with predicted_alp, predicted_tl, predicted_btl}
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
#' This function provides a complete workflow for SOM-based avalanche danger prediction:
#'
#' \strong{Workflow Steps:}
#' \enumerate{
#'   \item Split data into training and test sets
#'   \item Train a supersom model with FOUR layers (numeric, binary, danger, danger_prev)
#'   \item Map test observations using THREE layers (numeric, binary, danger_prev)
#'   \item Predict current danger levels based on mode of Layer 3 (danger) in training nodes
#'   \item Create confusion matrices for each elevation band
#'   \item Calculate performance metrics (precision, recall, F1, accuracy)
#' }
#'
#' \strong{SOM Architecture:}
#' \itemize{
#'   \item TRAINING (4 layers):
#'     \itemize{
#'       \item Layer 1: Numeric features (sum of squares distance)
#'       \item Layer 2: Binary features (Tanimoto distance)
#'       \item Layer 3: Current danger levels (sum of squares distance) - PREDICTION TARGET
#'       \item Layer 4: Previous danger levels (sum of squares distance) - PREDICTOR
#'     }
#'   \item TESTING (3 layers):
#'     \itemize{
#'       \item Layer 1: Numeric features
#'       \item Layer 2: Binary features
#'       \item Layer 3: Previous danger levels (NOT current - that's what we predict)
#'     }
#' }
#'
#' \strong{Prediction Strategy:}
#' The model learns the full pattern including current danger during training, but
#' at test time only has access to previous danger (plus other features). It predicts
#' current danger by finding similar historical patterns and using their outcomes.
#'
#' The function prints detailed progress information and comprehensive results
#' to the console, including confusion matrices and metrics for all three
#' elevation bands.
#'
#' @note
#' For temporal/time-series data, consider using \code{split_method = "sequential"}
#' or \code{\link{evaluate_som_prediction_presplit}} with
#' \code{\link{split_data_by_season}} for more robust temporal validation.
#'
#' @examples
#' \dontrun{
#' # Basic evaluation with random split
#' results <- evaluate_som_prediction(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,          # Current day (target)
#'   dataDangerPrev = my_danger_prev_data, # Previous day (predictor)
#'   xdim = 15,
#'   ydim = 11,
#'   test_size = 0.2,
#'   seed = 42
#' )
#'
#' # Sequential/temporal split
#' results <- evaluate_som_prediction(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,
#'   dataDangerPrev = my_danger_prev_data,
#'   split_method = "sequential"
#' )
#'
#' # Access results
#' model <- results$som_model
#' alp_accuracy <- results$metrics$alp$accuracy
#' alp_f1 <- results$metrics$alp$metrics$F1
#'
#' # View confusion matrix
#' print(results$confusion_matrices$alp)
#' }
#'
#' @seealso
#' \code{\link{split_data}}, \code{\link{predict_danger_from_som}},
#' \code{\link{evaluate_som_prediction_presplit}}, \code{\link{find_best_som}}
#'
#' @importFrom kohonen supersom somgrid
#' @export
evaluate_som_prediction <- function(dataNumeric, dataBinary, dataDanger, dataDangerPrev,
                                    xdim = 15, ydim = 11,
                                    test_size = 0.2, seed = 123,
                                    rlen = 1000, alpha = c(0.05, 0.01),
                                    split_method = "random") {
  
  cat("Starting SOM Predictive Performance Evaluation\n")
  cat("Training: 4 layers (numeric, binary, danger, danger_prev)\n")
  cat("Testing: 3 layers (numeric, binary, danger_prev)\n")
  cat("Target: Predict current danger from previous danger + features\n")
  cat("=" , rep("=", 50), "\n", sep = "")
  
  # 1. Split the data
  cat("\n1. Splitting data into train and test sets...\n")
  split <- split_data(dataNumeric, dataBinary, dataDanger, dataDangerPrev,
                      test_size, seed, split_method)
  cat("   Training observations:", length(split$train_indices), "\n")
  cat("   Test observations:", length(split$test_indices), "\n")
  
  # 2. Train the SOM with 4 layers
  cat("\n2. Training SOM model with 4 layers...\n")
  cat("   Layer 1: Numeric features\n")
  cat("   Layer 2: Binary features\n")
  cat("   Layer 3: Current danger (target)\n")
  cat("   Layer 4: Previous danger (predictor)\n")
  
  train_list <- list(
    numeric = as.matrix(split$train$numeric),
    binary = as.matrix(split$train$binary),
    danger = as.matrix(split$train$danger),
    danger_prev = as.matrix(split$train$danger_prev)
  )
  
  som_model <- kohonen::supersom(
    data = train_list,
    grid = kohonen::somgrid(xdim = xdim, ydim = ydim, topo = 'hexagonal'),
    rlen = rlen,
    alpha = alpha,
    dist.fcts = c("sumofsquares", "tanimoto", "sumofsquares", "sumofsquares"),
    keep.data = TRUE
  )
  cat("   Model trained successfully!\n")
  
  # 3. Predict on test data using 3 layers
  cat("\n3. Making predictions on test data...\n")
  cat("   Mapping with 3 layers: numeric, binary, danger_prev\n")
  cat("   Predicting current danger from node assignments\n")
  predictions <- predict_danger_from_som(som_model, split$test)
  
  # Actual values are CURRENT day's danger (the target)
  actual_alp <- split$test$danger$alp.used
  actual_tl <- split$test$danger$tl.used
  actual_btl <- split$test$danger$btl.used
  
  # Verify dimensions match
  cat("   Actual values length:", length(actual_alp), "\n")
  cat("   Predicted values length:", nrow(predictions), "\n")
  
  # 4. Create confusion matrices
  cat("\n4. Creating confusion matrices...\n")
  cm_alp <- create_confusion_matrix(actual_alp, predictions$predicted_alp)
  cm_tl <- create_confusion_matrix(actual_tl, predictions$predicted_tl)
  cm_btl <- create_confusion_matrix(actual_btl, predictions$predicted_btl)
  
  # 5. Calculate metrics
  cat("\n5. Calculating performance metrics...\n")
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
  
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("RESULTS: TL.USED\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nConfusion Matrix:\n")
  print(cm_tl)
  cat("\nPerformance Metrics:\n")
  print(metrics_tl$metrics, row.names = FALSE)
  cat("\nOverall Accuracy:", round(metrics_tl$accuracy, 4), "\n")
  
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("RESULTS: BTL.USED\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("\nConfusion Matrix:\n")
  print(cm_btl)
  cat("\nPerformance Metrics:\n")
  print(metrics_btl$metrics, row.names = FALSE)
  cat("\nOverall Accuracy:", round(metrics_btl$accuracy, 4), "\n")
  
  # Return all results
  return(list(
    som_model = som_model,
    split = split,
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
    )
  ))
}
