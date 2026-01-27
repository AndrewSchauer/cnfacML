#' Ensemble Predictions from Multiple SOM Models
#'
#' Generates ensemble predictions by combining predictions from multiple trained
#' SOM models using majority voting with conservative tie-breaking.
#'
#' @param som_models A list of trained supersom model objects.
#' @param test_data A list of matrices matching the format of matrices used in
#'   the layers that trained the super som (e.g., from $split_info$test). This
#'   should contain all data layers used for training except the prediction target.
#'   May contain more than two elements depending on how many layers were used.
#'   If test_data contains a 'danger' element, it will be automatically excluded
#'   to prevent duplication with predict_data.
#' @param predict_data A matrix of values that will be predicted by the model
#'   (e.g., danger levels). This should be a matrix with columns for different
#'   elevation bands (e.g., "alp.used", "tl.used", "btl.used"). Corresponds to
#'   the danger data that was included in the SOM training.
#' @param is_normalized Logical. If TRUE, assumes danger ratings in the model are
#'   normalized to 0-1 range and converts them back to 1-5 scale. If FALSE, assumes
#'   danger ratings are already in 1-5 scale. Default is FALSE.
#' @param verbose Logical. If TRUE, prints progress information. Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{ensemble_predictions}{Data frame with ensemble predictions:
#'       \itemize{
#'         \item predicted_alp: Ensemble alpine prediction
#'         \item predicted_tl: Ensemble treeline prediction
#'         \item predicted_btl: Ensemble below treeline prediction
#'       }
#'     }
#'     \item{individual_predictions}{List with matrices of individual model predictions:
#'       \itemize{
#'         \item alp: Matrix (n_test × n_models) of alpine predictions
#'         \item tl: Matrix (n_test × n_models) of treeline predictions
#'         \item btl: Matrix (n_test × n_models) of below treeline predictions
#'       }
#'     }
#'     \item{agreement}{Data frame with model agreement proportions for each observation:
#'       \itemize{
#'         \item alp_agreement: Proportion of models agreeing with ensemble (alpine)
#'         \item tl_agreement: Proportion of models agreeing with ensemble (treeline)
#'         \item btl_agreement: Proportion of models agreeing with ensemble (below treeline)
#'       }
#'     }
#'     \item{mean_agreement}{Data frame with mean agreement across all observations:
#'       \itemize{
#'         \item alp: Mean alpine agreement
#'         \item tl: Mean treeline agreement
#'         \item btl: Mean below treeline agreement
#'       }
#'     }
#'   }
#'
#' @details
#' \strong{Ensemble Method:}
#' The function uses majority voting to combine predictions:
#' \enumerate{
#'   \item Each model independently predicts danger levels for test data
#'   \item For each observation, the ensemble prediction is the mode (most frequent)
#'     across all models
#'   \item In case of ties, the highest danger level is selected (conservative)
#' }
#'
#' \strong{Model Agreement:}
#' Agreement measures how many models predicted the same value as the ensemble.
#' Higher agreement indicates more consensus among models, while lower agreement
#' suggests uncertainty.
#'
#' \strong{Conservative Tie-Breaking:}
#' When multiple danger levels have equal votes, the function selects the highest
#' level, which is the conservative and safer choice for avalanche forecasting.
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
#' This denormalization is applied to both training danger data from SOM models
#' and predict_data. All predictions are returned in the standard 1-5 scale.
#'
#' @note
#' This function requires \code{\link{get_mode_max_tie}} to be available.
#'
#' @examples
#' \dontrun{
#' # Train multiple models (e.g., from find_best_som_seasonal)
#' best_som_1 <- find_best_som_seasonal(data_list, date_column, seed = 1)
#' best_som_2 <- find_best_som_seasonal(data_list, date_column, seed = 100)
#' best_som_3 <- find_best_som_seasonal(data_list, date_column, seed = 200)
#'
#' # Create list of models
#' som_models <- list(
#'   best_som_1$best_model$som_model,
#'   best_som_2$best_model$som_model,
#'   best_som_3$best_model$som_model
#' )
#'
#' # Extract test data (all layers used for training)
#' test_data <- best_som_1$split_info$test
#'
#' # Extract prediction target (danger levels)
#' predict_data <- test_data$danger
#'
#' # Option 1: Remove danger from test_data before passing
#' ensemble <- ensemble_predict_som(
#'   som_models = som_models,
#'   test_data = test_data[names(test_data) != "danger"],
#'   predict_data = predict_data
#' )
#'
#' # Option 2: Pass full test_data (danger will be auto-removed)
#' ensemble <- ensemble_predict_som(
#'   som_models = som_models,
#'   test_data = test_data,  # includes danger element
#'   predict_data = predict_data  # danger will be excluded automatically
#' )
#'
#' # If using models trained on normalized data (0-1 range):
#' ensemble_normalized <- ensemble_predict_som(
#'   som_models = som_models,
#'   test_data = test_data,
#'   predict_data = predict_data,
#'   is_normalized = TRUE  # Converts 0-1 back to 1-5 scale
#' )
#'
#' # View ensemble predictions
#' head(ensemble$ensemble_predictions)
#'
#' # Check model agreement
#' print(ensemble$mean_agreement)
#' # High agreement (>80%) suggests stable predictions
#' # Low agreement (<60%) suggests uncertainty
#'
#' # Identify uncertain predictions
#' uncertain_obs <- which(ensemble$agreement$alp_agreement < 0.6)
#' }
#'
#' @seealso
#' \code{\link{get_mode_max_tie}}, \code{\link{evaluate_ensemble}},
#' \code{\link{plot_ensemble_distribution}}
#'
#' @importFrom kohonen map
#' @export
ensemble_predict_som <- function(som_models, test_data, predict_data, is_normalized = FALSE, verbose = TRUE) {

  # Helper function to denormalize danger values from 0-1 to 1-5
  denormalize_danger <- function(norm_value) {
    if (is.na(norm_value) || norm_value == 0) {
      return(NA)
    }
    # Convert from 0-1 normalized range back to 1-5 scale
    # Normalization scheme: 0.2=1, 0.4=2, 0.6=3, 0.8=4, 1.0=5
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
      cat("      Using provided predict_data argument instead.\n")
    }
    test_data <- test_data[names(test_data) != "danger"]
  }

  # Get number of observations from first element of test_data
  n_test <- nrow(test_data[[1]])

  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("ENSEMBLE PREDICTION FROM", n_models, "SOM MODELS\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\nTest observations:", n_test, "\n")
    cat("Number of models:", n_models, "\n")
    cat("Number of data layers:", length(test_data), "\n\n")
  }

  # Store predictions from each model
  all_predictions_alp <- matrix(0, nrow = n_test, ncol = n_models)
  all_predictions_tl <- matrix(0, nrow = n_test, ncol = n_models)
  all_predictions_btl <- matrix(0, nrow = n_test, ncol = n_models)

  # Convert predict_data to matrix if needed and ensure column names
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

  # Get predictions from each model
  for (m in 1:n_models) {
    if (verbose) {
      cat("Getting predictions from model", m, "...\n")
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

    # Get training data danger levels
    train_nodes <- som_model$unit.classif
    # The danger data is Layer 3 in the training data (current danger - the target)
    train_danger <- as.data.frame(som_model$data[[3]])

    # Ensure column names
    if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(train_danger))) {
      colnames(train_danger) <- c("alp.used", "tl.used", "btl.used")
    }

    # Denormalize danger values if needed
    if (is_normalized) {
      train_danger$alp.used <- denormalize_danger_vec(train_danger$alp.used)
      train_danger$tl.used <- denormalize_danger_vec(train_danger$tl.used)
      train_danger$btl.used <- denormalize_danger_vec(train_danger$btl.used)
    }

    # Predict for each test observation
    for (i in 1:n_test) {
      node <- test_nodes[i]
      node_train_indices <- which(train_nodes == node)

      if (length(node_train_indices) > 0) {
        all_predictions_alp[i, m] <- get_mode_max_tie(train_danger$alp.used[node_train_indices])
        all_predictions_tl[i, m] <- get_mode_max_tie(train_danger$tl.used[node_train_indices])
        all_predictions_btl[i, m] <- get_mode_max_tie(train_danger$btl.used[node_train_indices])
      } else {
        # Use overall mode if node is empty
        all_predictions_alp[i, m] <- get_mode_max_tie(train_danger$alp.used)
        all_predictions_tl[i, m] <- get_mode_max_tie(train_danger$tl.used)
        all_predictions_btl[i, m] <- get_mode_max_tie(train_danger$btl.used)
      }
    }
  }

  # Ensemble predictions: majority vote with tie-breaking (highest value wins)
  if (verbose) {
    cat("\nCreating ensemble predictions via majority vote...\n")
  }

  ensemble_alp <- apply(all_predictions_alp, 1, get_mode_max_tie)
  ensemble_tl <- apply(all_predictions_tl, 1, get_mode_max_tie)
  ensemble_btl <- apply(all_predictions_btl, 1, get_mode_max_tie)

  # Calculate model agreement (percentage of models that agree with ensemble)
  agreement_alp <- apply(all_predictions_alp, 1, function(row) sum(row == get_mode_max_tie(row)) / n_models)
  agreement_tl <- apply(all_predictions_tl, 1, function(row) sum(row == get_mode_max_tie(row)) / n_models)
  agreement_btl <- apply(all_predictions_btl, 1, function(row) sum(row == get_mode_max_tie(row)) / n_models)

  if (verbose) {
    cat("Ensemble predictions complete!\n")
    cat("\nMean model agreement:\n")
    cat("  ALP.USED : ", round(mean(agreement_alp) * 100, 1), "%\n", sep = "")
    cat("  TL.USED  : ", round(mean(agreement_tl) * 100, 1), "%\n", sep = "")
    cat("  BTL.USED : ", round(mean(agreement_btl) * 100, 1), "%\n", sep = "")
  }

  return(list(
    ensemble_predictions = data.frame(
      predicted_alp = ensemble_alp,
      predicted_tl = ensemble_tl,
      predicted_btl = ensemble_btl
    ),
    individual_predictions = list(
      alp = all_predictions_alp,
      tl = all_predictions_tl,
      btl = all_predictions_btl
    ),
    agreement = data.frame(
      alp_agreement = agreement_alp,
      tl_agreement = agreement_tl,
      btl_agreement = agreement_btl
    ),
    mean_agreement = data.frame(
      alp = mean(agreement_alp),
      tl = mean(agreement_tl),
      btl = mean(agreement_btl)
    )
  ))
}
