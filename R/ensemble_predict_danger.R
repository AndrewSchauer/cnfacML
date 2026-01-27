#' Predict Danger Levels Using SOM Ensemble with Probability Distributions
#'
#' Predicts avalanche danger levels for new observations using an ensemble of
#' trained SOM models. Provides both majority-vote predictions and detailed
#' probability distributions from each model's node assignments.
#'
#' @param som_models A list of trained supersom model objects.
#' @param test_data A list of matrices matching the format of matrices used in
#'   the layers that trained the super som. This should match the structure of
#'   the data used for training (e.g., from $split_info$test). May contain more
#'   than three elements depending on how many layers were used in training.
#'   If test_data contains a 'danger' element, it will be automatically excluded
#'   to prevent duplication with predict_data.
#'
#'   For 4-layer models, test_data should contain:
#'   \itemize{
#'     \item numeric: Numeric feature matrix
#'     \item binary: Binary feature matrix
#'     \item danger_prev: Previous day's danger (predictor)
#'   }
#'
#' @param predict_data OPTIONAL. A matrix of actual danger values for evaluation.
#'   Only needed when evaluating model performance against known danger levels.
#'   Should be a matrix with columns for different elevation bands
#'   (e.g., "alp.used", "tl.used", "btl.used").
#'
#'   \strong{For true prediction} (operational forecasting): Set to NULL or omit.
#'   \strong{For evaluation} (testing accuracy): Provide actual danger values.
#'
#' @param is_normalized Logical. If TRUE, assumes danger ratings in the model are
#'   normalized to 0-1 range and converts them back to 1-5 scale. If FALSE, assumes
#'   danger ratings are already in 1-5 scale. Default is FALSE.
#'
#' @param verbose Logical. If TRUE, prints progress information. Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{predictions}{Data frame with ensemble predictions (winning votes):
#'       \itemize{
#'         \item predicted_alp: Predicted alpine danger level (1-5)
#'         \item predicted_tl: Predicted treeline danger level (1-5)
#'         \item predicted_btl: Predicted below treeline danger level (1-5)
#'       }
#'     }
#'     \item{model_votes}{Data frame showing vote counts:
#'       \itemize{
#'         \item alp_votes_1 through alp_votes_5: Vote counts for each level
#'         \item tl_votes_1 through tl_votes_5: Vote counts for each level
#'         \item btl_votes_1 through btl_votes_5: Vote counts for each level
#'       }
#'     }
#'     \item{node_distributions}{List with distribution from each model's nodes:
#'       \itemize{
#'         \item model_1, model_2, ...: Each contains alp, tl, btl distributions
#'       }
#'     }
#'     \item{node_counts}{List with raw counts from each model's nodes:
#'       \itemize{
#'         \item model_1, model_2, ...: Each contains alp, tl, btl count vectors (length 5)
#'       }
#'     }
#'     \item{aggregated_counts}{Data frame with total training days across all activated nodes:
#'       \itemize{
#'         \item elevation: ALP, TL, or BTL
#'         \item danger_level: 1-5
#'         \item total_days: Sum of training observations at this level across all models/obs
#'         \item mean_days_per_model: Average days per model
#'       }
#'     }
#'     \item{average_distributions}{Data frame with averaged probabilities across models:
#'       \itemize{
#'         \item elevation: ALP, TL, or BTL
#'         \item danger_level: 1-5
#'         \item mean_probability: Average probability across all models
#'         \item sd_probability: Standard deviation of probabilities
#'       }
#'     }
#'     \item{plots}{List of ggplot objects:
#'       \itemize{
#'         \item model_distributions: Faceted plot showing each model's distribution
#'         \item model_distributions_with_counts: Same but with count labels
#'         \item average_distribution: Plot of averaged probabilities
#'         \item average_distribution_with_counts: Same but with count labels
#'         \item combined: Both plots together
#'         \item by_elevation: Separate plots for ALP, TL, BTL
#'       }
#'     }
#'   }
#'
#' @details
#' \strong{Two-Part Analysis:}
#' \enumerate{
#'   \item \strong{Majority Vote (Option A):} Each model votes for the mode danger
#'     level from its assigned node. The ensemble prediction is the majority vote
#'     with conservative tie-breaking.
#'   \item \strong{Distribution Analysis (Option B):} Each model provides the full
#'     probability distribution from its assigned node based on training data.
#'     These distributions are averaged across models and visualized.
#' }
#'
#' \strong{Probability Calculation:}
#' For each model and observation:
#' \itemize{
#'   \item Find the node the observation maps to
#'   \item Calculate the distribution of danger levels in that node's training data
#'   \item Example: If node has 10 obs with level 2, 5 with level 3, 2 with level 4:
#'     prob = {2: 59%, 3: 29%, 4: 12%}
#' }
#'
#' \strong{Count Tracking:}
#' The function tracks the number of training days (observations) at each danger
#' level for each node that gets activated. These counts are:
#' \itemize{
#'   \item Returned per-model in node_counts
#'   \item Aggregated across all models/observations in aggregated_counts
#'   \item Displayed in printed output
#'   \item Shown on plots with count labels
#' }
#'
#' \strong{Normalized Data Handling:}
#' When is_normalized = TRUE, the function converts danger ratings from 0-1
#' normalized range back to the standard 1-5 scale using the conversion:
#' \itemize{
#'   \item 0.2 -> 1 (Low)
#'   \item 0.4 -> 2 (Moderate)
#'   \item 0.6 -> 3 (Considerable)
#'   \item 0.8 -> 4 (High)
#'   \item 1.0 -> 5 (Extreme)
#' }
#' This denormalization is applied to:
#' \itemize{
#'   \item Training danger data from SOM models
#'   \item predict_data (if provided for evaluation)
#' }
#' All predictions and distributions are returned in the standard 1-5 scale.
#'
#' \strong{Missing Data Handling:}
#' If a node has no valid (non-NA, non-zero) danger data for an elevation band,
#' the function uses the overall distribution from the entire training set for
#' that elevation band.
#'
#' @examples
#' \dontrun{
#' # ========================================================================
#' # SCENARIO 1: EVALUATION MODE (testing model accuracy)
#' # ========================================================================
#' # You have actual danger ratings and want to test model performance
#'
#' # Create ensemble from multiple models
#' som_models <- list(
#'   model1$best_model$som_model,
#'   model2$best_model$som_model,
#'   model3$best_model$som_model
#' )
#'
#' # Get test data from find_best_som_seasonal output
#' test_data <- model1$split_info$test
#' actual_danger <- test_data$danger  # Known danger for evaluation
#'
#' # Run evaluation
#' results <- ensemble_predict_danger(
#'   som_models = som_models,
#'   test_data = test_data,
#'   predict_data = actual_danger  # Provide actual for evaluation
#' )
#'
#' # If using models trained on normalized data (0-1 range):
#' results_normalized <- ensemble_predict_danger(
#'   som_models = som_models,
#'   test_data = test_data,
#'   predict_data = actual_danger,
#'   is_normalized = TRUE  # Converts 0-1 back to 1-5 scale
#' )
#'
#' # Compare predictions to actual
#' accuracy <- mean(results$predictions$predicted_alp == actual_danger$alp.used)
#' cat("Alpine accuracy:", round(accuracy * 100, 2), "%\n")
#'
#' # View predictions
#' head(results$predictions)
#'
#' # View probability distributions
#' print(results$plots$average_distribution_with_counts)
#'
#' # ========================================================================
#' # SCENARIO 2: TRUE PREDICTION MODE (operational forecasting)
#' # ========================================================================
#' # You DON'T know the danger - you're making a forecast
#'
#' # Prepare today's data (no current danger, only predictors)
#' today_data <- list(
#'   numeric = today_numeric,      # Today's weather/snowpack data
#'   binary = today_binary,        # Today's avalanche problems
#'   danger_prev = yesterday_danger  # Yesterday's danger (known)
#' )
#'
#' # Make prediction (no predict_data needed!)
#' forecast <- ensemble_predict_danger(
#'   som_models = som_models,
#'   test_data = today_data,
#'   predict_data = NULL  # Or just omit this parameter
#' )
#'
#' # For models trained on normalized data:
#' forecast_normalized <- ensemble_predict_danger(
#'   som_models = som_models,
#'   test_data = today_data,
#'   is_normalized = TRUE  # Handles denormalization automatically
#' )
#'
#' # Get forecasted danger levels
#' cat("Alpine forecast:", forecast$predictions$predicted_alp, "\n")
#' cat("Treeline forecast:", forecast$predictions$predicted_tl, "\n")
#' cat("Below treeline forecast:", forecast$predictions$predicted_btl, "\n")
#'
#' # View uncertainty (probability distributions)
#' print(forecast$plots$average_distribution_with_counts)
#'
#' # See how models voted
#' print(forecast$model_votes)
#'
#' # ========================================================================
#' # SCENARIO 3: Multiple days forecast
#' # ========================================================================
#'
#' # Prepare data for next 3 days
#' forecast_data <- list(
#'   numeric = next_3days_numeric,
#'   binary = next_3days_binary,
#'   danger_prev = next_3days_prev_danger
#' )
#'
#' # Forecast (no predict_data)
#' forecast <- ensemble_predict_danger(
#'   som_models = som_models,
#'   test_data = forecast_data
#' )
#'
#' # View 3-day forecast
#' print(forecast$predictions)
#'
#' # View winning predictions
#' head(results$predictions)
#'
#' # View counts per model
#' results$node_counts$model_1
#'
#' # View aggregated counts
#' print(results$aggregated_counts)
#'
#' # Display plots
#' print(results$plots$model_distributions_with_counts)
#' print(results$plots$average_distribution_with_counts)
#' }
#'
#' @seealso \code{\link{ensemble_predict_som}}, \code{\link{get_mode_max_tie}}
#'
#' @importFrom kohonen map
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar geom_text facet_wrap facet_grid scale_fill_manual labs theme_minimal theme element_text ylim
#' @importFrom gridExtra grid.arrange
#' @export
ensemble_predict_danger <- function(som_models, test_data, predict_data = NULL, is_normalized = FALSE, verbose = TRUE) {

  # Helper function to denormalize danger values from 0-1 to 1-5
  denormalize_danger <- function(norm_value) {
    if (is.na(norm_value) || norm_value == 0) {
      return(NA)
    }
    # Convert from 0-1 normalized range back to 1-5 scale
    # Normalized: 0=NA/invalid, 0.25=1, 0.5=2, 0.75=3, 1.0=4 (assuming 0-4 was normalized)
    # OR: 0.2=1, 0.4=2, 0.6=3, 0.8=4, 1.0=5 (if 1-5 was normalized to 0-1)
    # We'll use the more common approach: (norm_value * 4) + 1 to get 1-5 range
    danger_val <- round((norm_value - 0.2) / 0.2) + 1
    danger_val <- pmax(1, pmin(5, danger_val))  # Ensure it's between 1 and 5
    return(danger_val)
  }

  # Vectorized version for applying to entire vectors
  denormalize_danger_vec <- function(norm_vec) {
    sapply(norm_vec, denormalize_danger)
  }

  if (verbose && is_normalized) {
    cat("\n=== NORMALIZED DATA MODE ===\n")
    cat("Converting danger values from 0-1 normalized range to 1-5 scale\n")
    cat("Normalization scheme: 0.2=1, 0.4=2, 0.6=3, 0.8=4, 1.0=5\n\n")
  }

  n_models <- length(som_models)

  # If test_data contains a 'danger' element, remove it to avoid duplication
  # This allows users to pass the full $split_info$test list
  if ("danger" %in% names(test_data)) {
    if (verbose) {
      cat("\nNote: 'danger' element found in test_data and will be excluded from mapping.\n")
      if (!is.null(predict_data)) {
        cat("      Using provided predict_data argument for evaluation.\n")
      }
    }
    test_data <- test_data[names(test_data) != "danger"]
  }

  # Get number of observations from first element of test_data
  n_obs <- nrow(test_data[[1]])

  # Determine if this is evaluation mode (predict_data provided) or prediction mode
  evaluation_mode <- !is.null(predict_data)

  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("ENSEMBLE DANGER PREDICTION WITH PROBABILITY DISTRIBUTIONS\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\nMode:", ifelse(evaluation_mode, "EVALUATION (with actual danger)", "PREDICTION (no actual danger)"), "\n")
    cat("Number of observations:", n_obs, "\n")
    cat("Number of models in ensemble:", n_models, "\n")
    cat("Number of data layers:", length(test_data), "\n\n")
  }

  # Store predictions (modes) from each model for voting
  all_predictions_alp <- matrix(0, nrow = n_obs, ncol = n_models)
  all_predictions_tl <- matrix(0, nrow = n_obs, ncol = n_models)
  all_predictions_btl <- matrix(0, nrow = n_obs, ncol = n_models)

  # Store probability distributions from each model
  all_distributions_alp <- array(0, dim = c(n_obs, 5, n_models))
  all_distributions_tl <- array(0, dim = c(n_obs, 5, n_models))
  all_distributions_btl <- array(0, dim = c(n_obs, 5, n_models))

  # Store counts from each model
  all_counts_alp <- array(0, dim = c(n_obs, 5, n_models))
  all_counts_tl <- array(0, dim = c(n_obs, 5, n_models))
  all_counts_btl <- array(0, dim = c(n_obs, 5, n_models))

  # Helper function to calculate distribution and counts, handling empty/NA cases
  calc_distribution_and_counts <- function(values, all_values) {
    # Filter out NA and 0 values
    valid_values <- values[!is.na(values) & values > 0]

    # If no valid values, use overall distribution from all data
    if (length(valid_values) == 0) {
      valid_values <- all_values[!is.na(all_values) & all_values > 0]
      # If still empty, return uniform distribution
      if (length(valid_values) == 0) {
        return(list(
          distribution = rep(0.2, 5),  # uniform across 5 levels
          counts = rep(0, 5)
        ))
      }
    }

    # Count occurrences of each level (1-5)
    counts <- sapply(1:5, function(x) sum(valid_values == x))

    # Calculate distribution
    distribution <- counts / sum(counts)

    return(list(
      distribution = distribution,
      counts = counts
    ))
  }

  # Convert predict_data to matrix only if provided (evaluation mode)
  if (evaluation_mode) {
    predict_data_mat <- as.matrix(predict_data)
    if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(predict_data_mat))) {
      colnames(predict_data_mat) <- c("alp.used", "tl.used", "btl.used")
    }

    # Denormalize predict_data if needed
    if (is_normalized) {
      predict_data_mat[, "alp.used"] <- denormalize_danger_vec(predict_data_mat[, "alp.used"])
      predict_data_mat[, "tl.used"] <- denormalize_danger_vec(predict_data_mat[, "tl.used"])
      predict_data_mat[, "btl.used"] <- denormalize_danger_vec(predict_data_mat[, "btl.used"])
    }
  }

  # Get predictions and distributions from each model
  for (m in 1:n_models) {
    if (verbose) {
      cat("Processing model", m, "...\n")
    }

    som_model <- som_models[[m]]

    # Convert test_data elements to matrices
    test_data_matrices <- lapply(test_data, as.matrix)

    # Map test data to SOM nodes using custom mapping that ignores Layer 3
    # For 4-layer models: numeric, binary, danger (IGNORED), danger_prev (actual)
    # Training was: Layer 1=numeric, Layer 2=binary, Layer 3=danger, Layer 4=danger_prev
    # Testing: Provide layers 1, 2, 4 and ignore layer 3 (current danger is UNKNOWN)
    test_predictions <- map_som_ignore_layers(
      som_model = som_model,
      newdata = list(
        test_data_matrices[[1]],  # Layer 1: numeric
        test_data_matrices[[2]],  # Layer 2: binary
        test_data_matrices[[3]]   # Layer 4: danger_prev (skipping layer 3)
      ),
      ignore_layers = 3  # Ignore Layer 3 (current danger)
    )

    test_nodes <- test_predictions$unit.classif

    # Get training data
    train_nodes <- som_model$unit.classif
    # The danger data is Layer 3 in the training data (current danger - the target)
    train_danger <- as.data.frame(som_model$data[[3]])

    # Ensure column names for danger data
    if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(train_danger))) {
      colnames(train_danger) <- c("alp.used", "tl.used", "btl.used")
    }

    # Denormalize danger values if needed
    if (is_normalized) {
      train_danger$alp.used <- denormalize_danger_vec(train_danger$alp.used)
      train_danger$tl.used <- denormalize_danger_vec(train_danger$tl.used)
      train_danger$btl.used <- denormalize_danger_vec(train_danger$btl.used)
    }

    # Overall distributions for fallback
    all_alp <- train_danger$alp.used
    all_tl <- train_danger$tl.used
    all_btl <- train_danger$btl.used

    # Predict for each test observation
    for (i in 1:n_obs) {
      node <- test_nodes[i]
      node_train_indices <- which(train_nodes == node)

      if (length(node_train_indices) > 0) {
        # Alpine
        node_alp <- train_danger$alp.used[node_train_indices]
        result_alp <- calc_distribution_and_counts(node_alp, all_alp)
        all_distributions_alp[i, , m] <- result_alp$distribution
        all_counts_alp[i, , m] <- result_alp$counts
        all_predictions_alp[i, m] <- get_mode_max_tie(node_alp[!is.na(node_alp) & node_alp > 0])

        # Treeline
        node_tl <- train_danger$tl.used[node_train_indices]
        result_tl <- calc_distribution_and_counts(node_tl, all_tl)
        all_distributions_tl[i, , m] <- result_tl$distribution
        all_counts_tl[i, , m] <- result_tl$counts
        all_predictions_tl[i, m] <- get_mode_max_tie(node_tl[!is.na(node_tl) & node_tl > 0])

        # Below Treeline
        node_btl <- train_danger$btl.used[node_train_indices]
        result_btl <- calc_distribution_and_counts(node_btl, all_btl)
        all_distributions_btl[i, , m] <- result_btl$distribution
        all_counts_btl[i, , m] <- result_btl$counts
        all_predictions_btl[i, m] <- get_mode_max_tie(node_btl[!is.na(node_btl) & node_btl > 0])

      } else {
        # Use overall distribution if node is empty
        result_alp <- calc_distribution_and_counts(c(), all_alp)
        result_tl <- calc_distribution_and_counts(c(), all_tl)
        result_btl <- calc_distribution_and_counts(c(), all_btl)

        all_distributions_alp[i, , m] <- result_alp$distribution
        all_distributions_tl[i, , m] <- result_tl$distribution
        all_distributions_btl[i, , m] <- result_btl$distribution

        all_counts_alp[i, , m] <- result_alp$counts
        all_counts_tl[i, , m] <- result_tl$counts
        all_counts_btl[i, , m] <- result_btl$counts

        all_predictions_alp[i, m] <- get_mode_max_tie(all_alp[!is.na(all_alp) & all_alp > 0])
        all_predictions_tl[i, m] <- get_mode_max_tie(all_tl[!is.na(all_tl) & all_tl > 0])
        all_predictions_btl[i, m] <- get_mode_max_tie(all_btl[!is.na(all_btl) & all_btl > 0])
      }
    }
  }

  # PART A: ENSEMBLE PREDICTIONS (MAJORITY VOTE)
  if (verbose) {
    cat("\nCreating ensemble predictions via majority vote...\n")
  }

  ensemble_alp <- apply(all_predictions_alp, 1, get_mode_max_tie)
  ensemble_tl <- apply(all_predictions_tl, 1, get_mode_max_tie)
  ensemble_btl <- apply(all_predictions_btl, 1, get_mode_max_tie)

  # Count votes for each danger level
  count_votes <- function(predictions) {
    votes <- matrix(0, nrow = nrow(predictions), ncol = 5)
    for (i in 1:nrow(predictions)) {
      for (level in 1:5) {
        votes[i, level] <- sum(predictions[i, ] == level)
      }
    }
    return(votes)
  }

  alp_votes <- count_votes(all_predictions_alp)
  tl_votes <- count_votes(all_predictions_tl)
  btl_votes <- count_votes(all_predictions_btl)

  colnames(alp_votes) <- paste0("alp_votes_", 1:5)
  colnames(tl_votes) <- paste0("tl_votes_", 1:5)
  colnames(btl_votes) <- paste0("btl_votes_", 1:5)

  if (verbose) {
    cat("Ensemble predictions complete!\n")
  }

  # PART B: PROBABILITY DISTRIBUTIONS
  if (verbose) {
    cat("\nCalculating probability distributions...\n")
  }

  # Average distributions across all observations and models
  model_distributions <- list()
  model_counts <- list()

  for (m in 1:n_models) {
    # Average across all observations for this model
    # Use drop=FALSE to preserve dimensions when n_obs=1
    if (n_obs == 1) {
      # For single observation, just use the values directly
      avg_alp <- all_distributions_alp[1, , m]
      avg_tl <- all_distributions_tl[1, , m]
      avg_btl <- all_distributions_btl[1, , m]

      sum_alp <- all_counts_alp[1, , m]
      sum_tl <- all_counts_tl[1, , m]
      sum_btl <- all_counts_btl[1, , m]
    } else {
      # For multiple observations, take column means
      avg_alp <- colMeans(all_distributions_alp[, , m])
      avg_tl <- colMeans(all_distributions_tl[, , m])
      avg_btl <- colMeans(all_distributions_btl[, , m])

      # Sum counts across all observations for this model
      sum_alp <- colSums(all_counts_alp[, , m])
      sum_tl <- colSums(all_counts_tl[, , m])
      sum_btl <- colSums(all_counts_btl[, , m])
    }

    model_distributions[[paste0("model_", m)]] <- list(
      alp = avg_alp,
      tl = avg_tl,
      btl = avg_btl
    )

    model_counts[[paste0("model_", m)]] <- list(
      alp = sum_alp,
      tl = sum_tl,
      btl = sum_btl
    )
  }

  # Calculate average distributions across models
  avg_alp_dist <- rowMeans(sapply(model_distributions, function(x) x$alp))
  avg_tl_dist <- rowMeans(sapply(model_distributions, function(x) x$tl))
  avg_btl_dist <- rowMeans(sapply(model_distributions, function(x) x$btl))

  # Calculate standard deviations
  sd_alp_dist <- apply(sapply(model_distributions, function(x) x$alp), 1, sd)
  sd_tl_dist <- apply(sapply(model_distributions, function(x) x$tl), 1, sd)
  sd_btl_dist <- apply(sapply(model_distributions, function(x) x$btl), 1, sd)

  # Aggregate counts across all models
  total_alp_counts <- rowSums(sapply(model_counts, function(x) x$alp))
  total_tl_counts <- rowSums(sapply(model_counts, function(x) x$tl))
  total_btl_counts <- rowSums(sapply(model_counts, function(x) x$btl))

  # Create aggregated counts data frame
  aggregated_counts <- data.frame(
    elevation = rep(c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"), each = 5),
    danger_level = rep(1:5, 3),
    total_days = c(total_alp_counts, total_tl_counts, total_btl_counts),
    mean_days_per_model = c(total_alp_counts, total_tl_counts, total_btl_counts) / n_models
  )

  # Create average distributions data frame
  avg_distributions <- data.frame(
    elevation = rep(c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"), each = 5),
    danger_level = rep(1:5, 3),
    mean_probability = c(avg_alp_dist, avg_tl_dist, avg_btl_dist),
    sd_probability = c(sd_alp_dist, sd_tl_dist, sd_btl_dist)
  )

  avg_distributions$elevation <- factor(avg_distributions$elevation,
                                        levels = c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"))

  # Print summary
  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("SUMMARY: AGGREGATED TRAINING DAYS BY DANGER LEVEL\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\nThese counts represent the total number of training days at each danger\n")
    cat("level across all nodes activated by test observations, summed across all models.\n\n")

    print(aggregated_counts)

    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("AVERAGE PROBABILITY DISTRIBUTIONS ACROSS MODELS\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\nThese probabilities are averaged across all", n_models, "models.\n")
    cat("They represent the typical distribution of danger levels in activated nodes.\n\n")

    print(avg_distributions[, c("elevation", "danger_level", "mean_probability", "sd_probability")])
  }

  # CREATE PLOTS
  if (verbose) {
    cat("\nCreating visualizations...\n")
  }

  # Define danger colors
  danger_colors <- c(
    "1" = "#4CAF50",  # Green - Low
    "2" = "#FFEB3B",  # Yellow - Moderate
    "3" = "#FF9800",  # Orange - Considerable
    "4" = "#F44336",  # Red - High
    "5" = "#231F20"   # Black - Extreme
  )

  # Prepare data for model distributions plot
  model_dist_data <- data.frame()
  for (m in 1:n_models) {
    for (elev in c("alp", "tl", "btl")) {
      elev_name <- switch(elev,
                          alp = "ALP (Alpine)",
                          tl = "TL (Treeline)",
                          btl = "BTL (Below Treeline)")

      temp_df <- data.frame(
        model = paste("Model", m),
        elevation = elev_name,
        danger_level = 1:5,
        probability = model_distributions[[m]][[elev]],
        count = model_counts[[m]][[elev]]
      )
      model_dist_data <- rbind(model_dist_data, temp_df)
    }
  }

  model_dist_data$elevation <- factor(model_dist_data$elevation,
                                      levels = c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"))

  # Plot 1: Individual model distributions (no counts)
  p_models <- ggplot2::ggplot(model_dist_data,
                              ggplot2::aes(x = factor(danger_level), y = probability,
                                           fill = factor(danger_level))) +
    ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
    ggplot2::facet_grid(elevation ~ model) +
    ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
    ggplot2::labs(title = "Probability Distributions by Model and Elevation Band",
                  x = "Danger Level",
                  y = "Probability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )

  # Plot 1b: Individual model distributions WITH counts
  p_models_counts <- ggplot2::ggplot(model_dist_data,
                                     ggplot2::aes(x = factor(danger_level), y = probability,
                                                  fill = factor(danger_level))) +
    ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(count > 0, paste0("n=", count), "")),
                       vjust = -0.3, size = 2.5) +
    ggplot2::facet_grid(elevation ~ model) +
    ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
    ggplot2::labs(title = "Probability Distributions by Model (with Training Day Counts)",
                  x = "Danger Level",
                  y = "Probability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )

  # Add aggregated counts to avg_distributions
  avg_distributions$total_days <- aggregated_counts$total_days

  # Separate data for each elevation (AFTER adding total_days)
  alp_avg_dist <- avg_distributions[avg_distributions$elevation == "ALP (Alpine)", ]
  tl_avg_dist <- avg_distributions[avg_distributions$elevation == "TL (Treeline)", ]
  btl_avg_dist <- avg_distributions[avg_distributions$elevation == "BTL (Below Treeline)", ]

  # Plot 2: Average distribution with error bars (no counts)
  p_average <- ggplot2::ggplot(avg_distributions,
                               ggplot2::aes(x = factor(danger_level), y = mean_probability,
                                            fill = factor(danger_level))) +
    ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, mean_probability - sd_probability),
                                        ymax = pmin(1, mean_probability + sd_probability)),
                           width = 0.3, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", mean_probability)),
                       vjust = -2, size = 3, fontface = "bold") +
    ggplot2::facet_wrap(~ elevation, ncol = 3) +
    ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
    ggplot2::labs(title = "Average Probability Distribution Across Models",
                  subtitle = "Error bars show ± 1 standard deviation",
                  x = "Danger Level",
                  y = "Mean Probability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::ylim(0, max(avg_distributions$mean_probability + avg_distributions$sd_probability) * 1.15)

  # Plot 2b: Average distribution WITH counts
  p_average_counts <- ggplot2::ggplot(avg_distributions,
                                      ggplot2::aes(x = factor(danger_level), y = mean_probability,
                                                   fill = factor(danger_level))) +
    ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, mean_probability - sd_probability),
                                        ymax = pmin(1, mean_probability + sd_probability)),
                           width = 0.3, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f\n(n=%d)", mean_probability, total_days)),
                       vjust = -1.5, size = 2.8, fontface = "bold") +
    ggplot2::facet_wrap(~ elevation, ncol = 3) +
    ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
    ggplot2::labs(title = "Average Probability Distribution (with Total Training Days)",
                  subtitle = "Error bars show ± 1 SD; counts show total days across all models",
                  x = "Danger Level",
                  y = "Mean Probability") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::ylim(0, max(avg_distributions$mean_probability + avg_distributions$sd_probability) * 1.2)

  # Plot 3: By elevation
  create_elevation_plot <- function(data, title) {
    ggplot2::ggplot(data, ggplot2::aes(x = factor(danger_level), y = mean_probability,
                                       fill = factor(danger_level))) +
      ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, mean_probability - sd_probability),
                                          ymax = pmin(1, mean_probability + sd_probability)),
                             width = 0.3, linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f\n±%.3f\n(n=%d)",
                                                      mean_probability, sd_probability, total_days)),
                         vjust = -0.5, size = 3.5, fontface = "bold") +
      ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
      ggplot2::labs(title = title, x = "Danger Level", y = "Mean Probability") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        legend.position = "none"
      ) +
      ggplot2::ylim(0, max(data$mean_probability + data$sd_probability) * 1.25)
  }

  p_alp <- create_elevation_plot(alp_avg_dist, "Alpine - Average Probability Distribution")
  p_tl <- create_elevation_plot(tl_avg_dist, "Treeline - Average Probability Distribution")
  p_btl <- create_elevation_plot(btl_avg_dist, "Below Treeline - Average Probability Distribution")

  # Combined plot
  p_combined <- gridExtra::grid.arrange(p_models_counts, p_average_counts, ncol = 1, heights = c(2, 1))

  if (verbose) {
    cat("\nEnsemble prediction complete!\n")
  }

  return(list(
    predictions = data.frame(
      predicted_alp = ensemble_alp,
      predicted_tl = ensemble_tl,
      predicted_btl = ensemble_btl
    ),
    model_votes = data.frame(alp_votes, tl_votes, btl_votes),
    node_distributions = model_distributions,
    node_counts = model_counts,
    aggregated_counts = aggregated_counts,
    average_distributions = avg_distributions,
    plots = list(
      model_distributions = p_models,
      model_distributions_with_counts = p_models_counts,
      average_distribution = p_average,
      average_distribution_with_counts = p_average_counts,
      combined = p_combined,
      by_elevation = list(alp = p_alp, tl = p_tl, btl = p_btl)
    )
  ))
}
