#' Map New Data to SOM with Option to Ignore Training Layers
#'
#' This function wraps kohonen::map() but allows you to specify which layers
#' from the trained SOM should be ignored during mapping. This is useful when
#' you trained with information that won't be available at prediction time.
#'
#' @param som_model A trained supersom model object from the kohonen package
#' @param newdata A list of data matrices to map (one per layer to USE, not including ignored layers)
#' @param ignore_layers Integer vector specifying which layer indices from the
#'   trained model should be ignored during mapping. For example, if you trained
#'   with 4 layers but want to ignore layer 3, set ignore_layers = 3.
#'
#' @return A kohonen mapping object with:
#'   \describe{
#'     \item{unit.classif}{Integer vector showing which node each observation maps to}
#'     \item{distances}{Matrix of distances from each observation to each node}
#'     \item{winning.distances}{Distance from each observation to its winning node}
#'     \item{grid}{The SOM grid structure}
#'   }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Extracting the codebook vectors from the trained SOM
#'   \item For each non-ignored layer, computing distances between newdata and codebook
#'   \item Summing distances across all used layers
#'   \item Assigning each observation to its closest node (minimum total distance)
#' }
#'
#' This allows you to train a SOM with all available information (including
#' the outcome variable) to get good clustering, but then map new observations
#' using only the predictor variables that are available at prediction time.
#'
#' \strong{Distance Functions Supported:}
#' \itemize{
#'   \item \code{sumofsquares}: Sum of squared differences
#'   \item \code{tanimoto}: Tanimoto distance for binary data
#'   \item \code{euclidean}: Euclidean distance
#'   \item \code{manhattan}: Manhattan (city block) distance
#' }
#'
#' @examples
#' \dontrun{
#' # Train with 4 layers including current danger
#' som_model <- supersom(
#'   data = list(numeric, binary, danger, danger_prev),
#'   grid = somgrid(10, 10),
#'   rlen = 1000,
#'   dist.fcts = c("sumofsquares", "tanimoto", "sumofsquares", "sumofsquares")
#' )
#'
#' # Map test data ignoring layer 3 (current danger)
#' # Only use layers 1, 2, and 4 (numeric, binary, danger_prev)
#' test_mapping <- map_som_ignore_layers(
#'   som_model = som_model,
#'   newdata = list(test_numeric, test_binary, test_danger_prev),
#'   ignore_layers = 3
#' )
#'
#' # Get node assignments
#' test_nodes <- test_mapping$unit.classif
#' }
#'
#' @seealso
#' \code{\link{predict_danger_from_som}}
#'
#' @export
map_som_ignore_layers <- function(som_model, newdata, ignore_layers = NULL) {
  
  # Get the trained codebook (SOM node prototypes)
  codes <- som_model$codes
  n_layers <- length(codes)
  n_nodes <- nrow(codes[[1]])
  
  # Get distance functions for each layer
  dist_fcts <- som_model$dist.fcts
  if (is.null(dist_fcts)) {
    # Default to sumofsquares for all layers
    dist_fcts <- rep("sumofsquares", n_layers)
  }
  
  # Validate inputs
  if (!is.null(ignore_layers)) {
    if (any(ignore_layers < 1 | ignore_layers > n_layers)) {
      stop("ignore_layers must be between 1 and ", n_layers)
    }
    
    # Check that newdata has the right number of layers
    expected_layers <- n_layers - length(ignore_layers)
    if (length(newdata) != expected_layers) {
      stop("newdata should have ", expected_layers, " layers (", n_layers, 
           " training layers - ", length(ignore_layers), " ignored layers)")
    }
    
    # Create index mapping: which newdata layer corresponds to which training layer
    training_layers_used <- setdiff(1:n_layers, ignore_layers)
  } else {
    training_layers_used <- 1:n_layers
  }
  
  # Number of observations to map
  n_obs <- nrow(as.matrix(newdata[[1]]))
  
  # Initialize distance matrix: rows = observations, cols = nodes
  total_distances <- matrix(0, nrow = n_obs, ncol = n_nodes)
  
  # Calculate distances for each layer (excluding ignored layers)
  for (i in seq_along(newdata)) {
    training_layer_idx <- training_layers_used[i]
    
    # Get codebook for this layer
    layer_codes <- as.matrix(codes[[training_layer_idx]])
    
    # Get new data for this layer
    layer_data <- as.matrix(newdata[[i]])
    
    # Get distance function for this layer
    dist_fct <- dist_fcts[training_layer_idx]
    
    # Calculate distances between each observation and each node
    layer_distances <- .compute_layer_distances(
      data = layer_data,
      codes = layer_codes,
      dist_fct = dist_fct
    )
    
    # Add to total distances
    total_distances <- total_distances + layer_distances
  }
  
  # Find closest node for each observation (minimum distance)
  unit.classif <- apply(total_distances, 1, which.min)
  
  # Calculate distances to winning nodes
  winning.distances <- numeric(n_obs)
  for (i in 1:n_obs) {
    winning.distances[i] <- total_distances[i, unit.classif[i]]
  }
  
  # Return result in similar format to kohonen::map
  result <- list(
    unit.classif = unit.classif,
    distances = total_distances,
    winning.distances = winning.distances,
    grid = som_model$grid
  )
  
  class(result) <- c("kohonen", "list")
  return(result)
}


#' Compute Distance Between Data and Codebook for a Single Layer
#'
#' Internal helper function for map_som_ignore_layers. Calculates distances
#' between observations and SOM codebook vectors for a single layer.
#'
#' @param data Matrix of observations (rows) x variables (cols)
#' @param codes Matrix of codebook vectors (nodes) x variables (cols)
#' @param dist_fct Distance function name: "sumofsquares", "tanimoto", "euclidean", "manhattan"
#'
#' @return Matrix of distances: rows = observations, cols = nodes
#'
#' @keywords internal
#' @noRd
.compute_layer_distances <- function(data, codes, dist_fct) {
  
  n_obs <- nrow(data)
  n_nodes <- nrow(codes)
  distances <- matrix(0, nrow = n_obs, ncol = n_nodes)
  
  if (dist_fct == "sumofsquares") {
    # Sum of squared differences
    for (j in 1:n_nodes) {
      # For each node, calculate squared differences with all observations
      diff <- sweep(data, 2, codes[j, ], "-")
      distances[, j] <- rowSums(diff^2, na.rm = TRUE)
    }
    
  } else if (dist_fct == "tanimoto") {
    # Tanimoto distance for binary data
    for (j in 1:n_nodes) {
      code_vector <- codes[j, ]
      for (i in 1:n_obs) {
        data_vector <- data[i, ]
        
        # Remove NAs
        valid <- !is.na(data_vector) & !is.na(code_vector)
        if (sum(valid) == 0) {
          distances[i, j] <- NA
        } else {
          dv <- data_vector[valid]
          cv <- code_vector[valid]
          
          # Tanimoto: 1 - (sum of mins) / (sum of maxs)
          distances[i, j] <- 1 - sum(pmin(dv, cv)) / sum(pmax(dv, cv))
        }
      }
    }
    
  } else if (dist_fct == "euclidean") {
    # Euclidean distance (square root of sumofsquares)
    for (j in 1:n_nodes) {
      diff <- sweep(data, 2, codes[j, ], "-")
      distances[, j] <- sqrt(rowSums(diff^2, na.rm = TRUE))
    }
    
  } else if (dist_fct == "manhattan") {
    # Manhattan distance (sum of absolute differences)
    for (j in 1:n_nodes) {
      diff <- sweep(data, 2, codes[j, ], "-")
      distances[, j] <- rowSums(abs(diff), na.rm = TRUE)
    }
    
  } else {
    stop("Unsupported distance function: ", dist_fct)
  }
  
  return(distances)
}
