# Source the custom mapping function
source_if_needed <- function(file) {
  if (!exists("map_som_ignore_layers")) {
    source(file)
  }
}

predict_danger_from_som <- function(som_model, test_data) {

  # Source the custom mapping function if not already loaded
  tryCatch({
    source_if_needed("map_som_ignore_layers.R")
  }, error = function(e) {
    stop("map_som_ignore_layers.R must be sourced. Please run: source('map_som_ignore_layers.R')")
  })

  # For 4-layer trained models, we map using only layers 1, 2, and 4
  # Layer 3 (current danger) is IGNORED because it's UNKNOWN at test time
  # This allows the SOM to cluster using all information during training,
  # but map new observations using only what's available at prediction time

  # Check if we have danger_prev in test_data (4-layer training scenario)
  if("danger_prev" %in% names(test_data)) {
    # Use custom mapping function that ignores layer 3 (danger)
    # Provide newdata in the order: layer 1, layer 2, layer 4 (skipping layer 3)
    test_predictions <- map_som_ignore_layers(
      som_model = som_model,
      newdata = list(
        as.matrix(test_data$numeric),      # Layer 1
        as.matrix(test_data$binary),       # Layer 2
        as.matrix(test_data$danger_prev)   # Layer 4 (skipping layer 3)
      ),
      ignore_layers = 3  # Ignore layer 3 (current danger)
    )
  } else if("danger" %in% names(test_data)) {
    # Old 3-layer format (backward compatibility)
    test_predictions <- kohonen::map(som_model, newdata = list(
      numeric = as.matrix(test_data$numeric),
      binary = as.matrix(test_data$binary),
      danger = as.matrix(test_data$danger)
    ))
  } else {
    stop("test_data must contain either 'danger' or 'danger_prev'")
  }

  # Get the node assignments for test data
  test_nodes <- test_predictions$unit.classif

  # Get training data danger levels from Layer 3 (current danger - the target)
  train_nodes <- som_model$unit.classif
  train_danger <- as.data.frame(som_model$data[[3]])  # Layer 3 is current danger

  # Ensure column names match
  if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(train_danger))) {
    colnames(train_danger) <- c("alp.used", "tl.used", "btl.used")
  }

  # For each test observation, predict danger based on the mode
  n_test <- length(test_nodes)
  predicted_alp <- numeric(n_test)
  predicted_tl <- numeric(n_test)
  predicted_btl <- numeric(n_test)

  for (i in 1:n_test) {
    node <- test_nodes[i]
    node_train_indices <- which(train_nodes == node)

    if (length(node_train_indices) > 0) {
      node_alp <- train_danger$alp.used[node_train_indices]
      node_tl <- train_danger$tl.used[node_train_indices]
      node_btl <- train_danger$btl.used[node_train_indices]

      predicted_alp[i] <- get_mode_max_tie(node_alp)
      predicted_tl[i] <- get_mode_max_tie(node_tl)
      predicted_btl[i] <- get_mode_max_tie(node_btl)
    } else {
      predicted_alp[i] <- get_mode_max_tie(train_danger$alp.used)
      predicted_tl[i] <- get_mode_max_tie(train_danger$tl.used)
      predicted_btl[i] <- get_mode_max_tie(train_danger$btl.used)
    }
  }

  cat("   Predictions generated for", n_test, "test observations\n")
  if("danger_prev" %in% names(test_data)) {
    cat("   (Mapped using layers 1, 2, 4: numeric, binary, danger_prev)\n")
    cat("   (Layer 3 - current danger - IGNORED during mapping)\n")
  } else {
    cat("   (Mapped using: numeric, binary, danger)\n")
  }
  cat("   (Predicted from: Layer 3 danger values in training nodes)\n")

  return(data.frame(
    predicted_alp = predicted_alp,
    predicted_tl = predicted_tl,
    predicted_btl = predicted_btl
  ))
}
