#' Diagnose Ensemble Prediction Issues
#'
#' Detailed diagnostic output for ensemble danger predictions to help identify
#' why predictions might not match expectations. Works with 4-layer trained models
#' and 3-layer testing.
#'
#' @param som_models A list of trained supersom model objects (trained with 4 layers).
#' @param dataNumeric A matrix or data frame of numeric predictor variables.
#' @param dataBinary A matrix or data frame of binary predictor variables.
#' @param dataDangerPrev A matrix or data frame of PREVIOUS day's danger levels.
#' @param obs_index Which observation to diagnose (default 1).
#'
#' @return Prints diagnostic information and returns a list with details.
#'
#' @export
diagnose_ensemble_prediction <- function(som_models, dataNumeric, dataBinary, 
                                        dataDangerPrev, obs_index = 1) {
  
  n_models <- length(som_models)
  n_obs <- nrow(dataNumeric)
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("ENSEMBLE PREDICTION DIAGNOSTICS\n")
  cat("Models trained with: 4 layers (numeric, binary, danger, danger_prev)\n")
  cat("Mapping test data with: 3 layers (numeric, binary, danger_prev)\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nDiagnosing observation", obs_index, "of", n_obs, "\n")
  cat("Number of models:", n_models, "\n\n")
  
  all_node_info <- list()
  
  for (m in 1:n_models) {
    cat("\n", rep("-", 70), "\n", sep = "")
    cat("MODEL", m, "\n")
    cat(rep("-", 70), "\n", sep = "")
    
    som_model <- som_models[[m]]
    
    # Map new data using custom mapping that ignores Layer 3 (current danger)
    new_mappings <- map_som_ignore_layers(
      som_model = som_model,
      newdata = list(
        as.matrix(dataNumeric),      # Layer 1
        as.matrix(dataBinary),        # Layer 2
        as.matrix(dataDangerPrev)     # Layer 4 (skipping layer 3)
      ),
      ignore_layers = 3  # Ignore Layer 3 (current danger)
    )
    
    node <- new_mappings$unit.classif[obs_index]
    cat("Observation", obs_index, "maps to node:", node, "\n")
    
    # Get training data
    train_nodes <- som_model$unit.classif
    # Layer 3 is current danger (the prediction target from training)
    train_danger <- as.data.frame(som_model$data[[3]])
    
    if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(train_danger))) {
      colnames(train_danger) <- c("alp.used", "tl.used", "btl.used")
    }
    
    # Get training samples in this node
    node_train_indices <- which(train_nodes == node)
    cat("Training samples in node:", length(node_train_indices), "\n")
    cat("(These are the CURRENT danger values from training data)\n\n")
    
    if (length(node_train_indices) > 0) {
      # Alpine
      node_alp <- train_danger$alp.used[node_train_indices]
      valid_alp <- node_alp[!is.na(node_alp) & node_alp > 0]
      
      cat("ALPINE:\n")
      cat("  Raw values:", paste(head(node_alp, 20), collapse=", "), 
          if(length(node_alp) > 20) "..." else "", "\n")
      cat("  Valid values (non-NA, >0):", length(valid_alp), "\n")
      if (length(valid_alp) > 0) {
        alp_table <- table(valid_alp)
        cat("  Distribution:\n")
        for (lvl in names(alp_table)) {
          cat("    Level", lvl, ":", alp_table[lvl], "days (",
              round(100 * alp_table[lvl] / sum(alp_table), 1), "%)\n")
        }
        cat("  Mode (prediction):", get_mode_max_tie(valid_alp), "\n")
      } else {
        cat("  WARNING: No valid values - using overall training distribution\n")
        overall_alp <- train_danger$alp.used[!is.na(train_danger$alp.used) & train_danger$alp.used > 0]
        cat("  Overall training mode:", get_mode_max_tie(overall_alp), "\n")
      }
      
      # Treeline
      node_tl <- train_danger$tl.used[node_train_indices]
      valid_tl <- node_tl[!is.na(node_tl) & node_tl > 0]
      
      cat("\nTREELINE:\n")
      cat("  Valid values:", length(valid_tl), "\n")
      if (length(valid_tl) > 0) {
        tl_table <- table(valid_tl)
        cat("  Distribution:\n")
        for (lvl in names(tl_table)) {
          cat("    Level", lvl, ":", tl_table[lvl], "days (",
              round(100 * tl_table[lvl] / sum(tl_table), 1), "%)\n")
        }
        cat("  Mode (prediction):", get_mode_max_tie(valid_tl), "\n")
      } else {
        cat("  WARNING: No valid values - using overall training distribution\n")
      }
      
      # Below Treeline
      node_btl <- train_danger$btl.used[node_train_indices]
      valid_btl <- node_btl[!is.na(node_btl) & node_btl > 0]
      
      cat("\nBELOW TREELINE:\n")
      cat("  Valid values:", length(valid_btl), "\n")
      if (length(valid_btl) > 0) {
        btl_table <- table(valid_btl)
        cat("  Distribution:\n")
        for (lvl in names(btl_table)) {
          cat("    Level", lvl, ":", btl_table[lvl], "days (",
              round(100 * btl_table[lvl] / sum(btl_table), 1), "%)\n")
        }
        cat("  Mode (prediction):", get_mode_max_tie(valid_btl), "\n")
      } else {
        cat("  WARNING: No valid values - using overall training distribution\n")
      }
      
      all_node_info[[paste0("model_", m)]] <- list(
        node = node,
        n_train_in_node = length(node_train_indices),
        alp_distribution = if(length(valid_alp) > 0) table(valid_alp) else NULL,
        tl_distribution = if(length(valid_tl) > 0) table(valid_tl) else NULL,
        btl_distribution = if(length(valid_btl) > 0) table(valid_btl) else NULL,
        alp_prediction = if(length(valid_alp) > 0) get_mode_max_tie(valid_alp) else get_mode_max_tie(overall_alp),
        tl_prediction = if(length(valid_tl) > 0) get_mode_max_tie(valid_tl) else NA,
        btl_prediction = if(length(valid_btl) > 0) get_mode_max_tie(valid_btl) else NA
      )
    } else {
      cat("\nWARNING: Node is empty (no training data)\n")
      cat("Will use overall training distribution for predictions\n")
      
      all_node_info[[paste0("model_", m)]] <- list(
        node = node,
        n_train_in_node = 0,
        warning = "Empty node"
      )
    }
  }
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("SUMMARY\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nNote: Predictions are based on CURRENT danger (Layer 3) from training\n")
  cat("      data in the nodes that test observations map to using 3 layers.\n")
  
  invisible(all_node_info)
}
