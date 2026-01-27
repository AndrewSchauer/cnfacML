#' Find Training Days Similar to Test Observation
#'
#' Searches training data for days with similar features but different danger
#' levels. Helps identify if the SOM prediction conflicts with similar historical
#' examples.
#'
#' @param som_model A trained supersom model object.
#' @param dataNumeric_test Test observation numeric features (single row).
#' @param dataBinary_test Test observation binary features (single row).
#' @param feature_subset Character vector of key features to focus on (optional).
#' @param n_similar Number of similar days to show (default 20).
#'
#' @return Prints similar training days and their danger levels.
#'
#' @export
find_similar_training_days <- function(som_model, dataNumeric_test, dataBinary_test, 
                                       feature_subset = NULL, n_similar = 20) {
  
  # Get training data
  train_numeric <- as.data.frame(som_model$data[[1]])
  train_binary <- as.data.frame(som_model$data[[2]])
  train_danger <- as.data.frame(som_model$data[[3]])
  
  if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(train_danger))) {
    colnames(train_danger) <- c("alp.used", "tl.used", "btl.used")
  }
  
  n_train <- nrow(train_numeric)
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("FINDING SIMILAR TRAINING DAYS\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  
  # If feature subset specified, use only those
  if (!is.null(feature_subset)) {
    numeric_features <- intersect(feature_subset, colnames(train_numeric))
    binary_features <- intersect(feature_subset, colnames(train_binary))
    cat("\nFocusing on features:", paste(c(numeric_features, binary_features), collapse=", "), "\n")
  } else {
    numeric_features <- colnames(train_numeric)
    binary_features <- colnames(train_binary)
    cat("\nUsing all features\n")
  }
  
  # Ensure test data is a data frame with correct column names
  if (!is.data.frame(dataNumeric_test)) {
    dataNumeric_test <- as.data.frame(dataNumeric_test)
    colnames(dataNumeric_test) <- colnames(train_numeric)
  }
  if (!is.data.frame(dataBinary_test)) {
    dataBinary_test <- as.data.frame(dataBinary_test)
    colnames(dataBinary_test) <- colnames(train_binary)
  }
  
  # Calculate distances (simple Euclidean for numeric, Hamming for binary)
  distances <- numeric(n_train)
  
  # Normalize numeric features
  test_numeric <- dataNumeric_test[1, numeric_features, drop = FALSE]
  train_numeric_subset <- train_numeric[, numeric_features, drop = FALSE]
  
  for (feat in numeric_features) {
    feat_sd <- sd(train_numeric_subset[, feat], na.rm = TRUE)
    if (feat_sd > 0) {
      test_val <- (test_numeric[1, feat] - mean(train_numeric_subset[, feat], na.rm = TRUE)) / feat_sd
      train_vals <- (train_numeric_subset[, feat] - mean(train_numeric_subset[, feat], na.rm = TRUE)) / feat_sd
      distances <- distances + (test_val - train_vals)^2
    }
  }
  
  # Add binary features (Hamming distance)
  test_binary <- dataBinary_test[1, binary_features, drop = FALSE]
  train_binary_subset <- train_binary[, binary_features, drop = FALSE]
  
  for (feat in binary_features) {
    distances <- distances + abs(test_binary[1, feat] - train_binary_subset[, feat])
  }
  
  distances <- sqrt(distances)
  
  # Find most similar
  similar_indices <- order(distances)[1:n_similar]
  
  cat("\nTest observation features:\n")
  cat(rep("-", 70), "\n", sep = "")
  
  # Show key numeric features
  key_numeric <- if (!is.null(feature_subset)) numeric_features else colnames(test_numeric)[1:min(10, ncol(test_numeric))]
  cat("\nKey numeric features:\n")
  for (feat in key_numeric) {
    cat("  ", feat, ": ", round(test_numeric[1, feat], 2), "\n", sep = "")
  }
  
  # Show active binary features
  active_binary <- colnames(test_binary)[test_binary[1, ] == 1]
  cat("\nActive problem types:\n")
  if (length(active_binary) > 0) {
    for (feat in active_binary) {
      cat("  ", feat, "\n", sep = "")
    }
  } else {
    cat("  (none)\n")
  }
  
  # Show similar training days
  cat("\n\nMOST SIMILAR TRAINING DAYS:\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("%-8s %-10s %-8s %-8s %-8s", "Rank", "Distance", "ALP", "TL", "BTL"))
  
  # Add key feature columns
  display_features <- if (!is.null(feature_subset)) {
    intersect(feature_subset, colnames(train_numeric))[1:min(3, length(intersect(feature_subset, colnames(train_numeric))))]
  } else {
    colnames(train_numeric)[1:min(3, ncol(train_numeric))]
  }
  
  for (feat in display_features) {
    cat(sprintf(" %-12s", substr(feat, 1, 12)))
  }
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  
  # Count danger levels in similar days
  danger_counts <- list(
    alp = table(factor(train_danger$alp.used[similar_indices], levels = 1:5)),
    tl = table(factor(train_danger$tl.used[similar_indices], levels = 1:5)),
    btl = table(factor(train_danger$btl.used[similar_indices], levels = 1:5))
  )
  
  for (i in 1:length(similar_indices)) {
    idx <- similar_indices[i]
    
    cat(sprintf("%-8d %-10.3f %-8s %-8s %-8s", 
                i, 
                distances[idx],
                train_danger$alp.used[idx],
                train_danger$tl.used[idx],
                train_danger$btl.used[idx]))
    
    for (feat in display_features) {
      cat(sprintf(" %-12.2f", train_numeric[idx, feat]))
    }
    cat("\n")
  }
  
  # Summary
  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("DANGER LEVEL DISTRIBUTION IN", n_similar, "MOST SIMILAR DAYS:\n")
  cat(rep("=", 70), "\n", sep = "")
  
  cat("\nAlpine:\n")
  for (lvl in 1:5) {
    if (danger_counts$alp[lvl] > 0) {
      pct <- round(100 * danger_counts$alp[lvl] / n_similar, 1)
      cat("  Level ", lvl, ": ", danger_counts$alp[lvl], " days (", pct, "%)\n", sep = "")
    }
  }
  
  cat("\nTreeline:\n")
  for (lvl in 1:5) {
    if (danger_counts$tl[lvl] > 0) {
      pct <- round(100 * danger_counts$tl[lvl] / n_similar, 1)
      cat("  Level ", lvl, ": ", danger_counts$tl[lvl], " days (", pct, "%)\n", sep = "")
    }
  }
  
  cat("\nBelow Treeline:\n")
  for (lvl in 1:5) {
    if (danger_counts$btl[lvl] > 0) {
      pct <- round(100 * danger_counts$btl[lvl] / n_similar, 1)
      cat("  Level ", lvl, ": ", danger_counts$btl[lvl], " days (", pct, "%)\n", sep = "")
    }
  }
  
  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("INTERPRETATION:\n")
  cat("If most similar days are Level 1, the SOM prediction is consistent.\n")
  cat("If many are Level 2-3, there may be an issue with node assignment.\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  
  invisible(list(
    similar_indices = similar_indices,
    distances = distances[similar_indices],
    danger_levels = train_danger[similar_indices, ]
  ))
}
