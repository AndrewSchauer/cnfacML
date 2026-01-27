#' Iterative SOM Training with Performance Metrics
#'
#' Trains multiple SOM models with identical parameters but different random
#' initializations, saving each model and tracking within-cluster and between-cluster
#' distances.
#'
#' @param data_in A list of data matrices/data frames to be used for SOM training
#'   (e.g., list(numeric = ..., binary = ..., danger = ...)).
#' @param xdim Integer. Width of the SOM grid.
#' @param ydim Integer. Height of the SOM grid.
#' @param rlen Integer. Number of training iterations.
#' @param alpha Numeric vector of length 2. Learning rate range c(start, end).
#' @param num.tests Integer. Number of SOM models to train. Default is 10.
#' @param distance Character vector. Distance functions for each data layer
#'   (e.g., c("sumofsquares", "tanimoto", "sumofsquares")).
#' @param topo Character. Topology type: "hexagonal" (default) or "rectangular".
#'
#' @return A SOMiterator object with attributes:
#'   \describe{
#'     \item{Summary}{Data frame with Within and Between distances for each iteration}
#'     \item{nodes}{Total number of nodes (xdim * ydim)}
#'     \item{rlen}{Number of training iterations used}
#'     \item{Distance}{Distance functions used}
#'     \item{LearnRate}{Learning rate (alpha) used}
#'   }
#'
#' @details
#' \strong{Workflow:}
#' \enumerate{
#'   \item Train \code{num.tests} SOM models with same parameters
#'   \item Each model saved to disk with unique filename
#'   \item Calculate quality metrics for each model:
#'     \itemize{
#'       \item Within-cluster distance (data points to their BMU)
#'       \item Between-cluster distance (distances between node prototypes)
#'     }
#' }
#'
#' \strong{Distance Metrics:}
#' \itemize{
#'   \item For "sumofsquares": Uses RMSE (Root Mean Squared Error)
#'   \item For other distances: Uses mean Euclidean distance
#' }
#'
#' \strong{File Naming:}
#' Models are saved as: \code{SOM_<nodes>_a<alpha>_<iteration>_<rlen>.RData}
#'
#' Example: \code{SOM_165_a5_1_1000.RData} for 165 nodes, alpha=0.05, iteration 1, rlen=1000
#'
#' \strong{Quality Interpretation:}
#' \itemize{
#'   \item Lower within-cluster distance = better data fit
#'   \item Higher between-cluster distance = better separation
#'   \item Compare across iterations to assess stability
#' }
#'
#' @note
#' This function saves files to a hard-coded path. You may need to modify
#' the save path in the function code to match your directory structure.
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' data_list <- list(
#'   numeric = as.matrix(dataNumeric),
#'   binary = as.matrix(dataBinary),
#'   danger = as.matrix(dataDanger)
#' )
#'
#' # Run iterator
#' som_iter <- SOMiterator(
#'   data_in = data_list,
#'   xdim = 15,
#'   ydim = 11,
#'   rlen = 1000,
#'   alpha = c(0.05, 0.01),
#'   num.tests = 10,
#'   distance = c("sumofsquares", "tanimoto", "sumofsquares")
#' )
#'
#' # View summary
#' summary_df <- attr(som_iter, "Summary")
#' print(summary_df)
#'
#' # Plot quality metrics
#' plot(summary_df$Within, type = "b", main = "Within-Cluster Distance")
#' plot(summary_df$Between, type = "b", main = "Between-Cluster Distance")
#'
#' # Find best iteration
#' best_iter <- which.min(summary_df$Within)
#' cat("Best iteration:", best_iter, "\n")
#' }
#'
#' @seealso \code{\link[kohonen]{supersom}}, \code{\link{find_best_som}}
#'
#' @importFrom kohonen supersom somgrid
#' @export
SOMiterator <- function(data_in, xdim, ydim, rlen, alpha, num.tests = 10, distance, topo = 'hexagonal') {
  
  nNodes <- xdim * ydim
  testgrid <- kohonen::somgrid(xdim = xdim, ydim = ydim, topo = topo)
  betweenDist <- withinDist <- rep(0, num.tests)
  
  for (it in 1:num.tests) {
    SOMit <- kohonen::supersom(
      data = data_in,
      grid = testgrid,
      rlen = rlen,
      alpha = alpha,
      dist.fcts = distance,
      keep.data = TRUE
    )
    
    # Save each iteration
    # NOTE: Update this path to match your directory structure
    save(SOMit, file = paste("./results/SOM",
                             nNodes,
                             "_a",
                             alpha[1] * 100,
                             "_",
                             it,
                             "_",
                             rlen,
                             ".RData",
                             sep = ""))
    print(paste("SOM iteration", it, "complete"))
    
    # Calculate within-cluster distance
    withinDist[it] <- ifelse(
      SOMit$dist.fcts[1] == "sumofsquares",
      sqrt(sum(SOMit$distances) / nrow(data_in[[1]])),  # RMSE
      sum(SOMit$distances) / nrow(data_in[[1]])         # Mean Euclidean distance
    )
    
    # Calculate between-cluster distance
    betweenDist[it] <- ifelse(
      SOMit$dist.fcts[1] == "sumofsquares",
      sqrt(mean((as.matrix(dist(SOMit$codes[[1]])))^2)),  # RMSE between nodes
      mean(dist(SOMit$codes[[1]]))                         # Mean distance between nodes
    )
    
    print(paste("Metrics calculated for iteration", it))
  }
  
  # Create summary data frame
  Summ <- data.frame("Within" = withinDist, "Between" = betweenDist)
  
  # Create return object with attributes
  result <- list()
  attr(result, "Summary") <- Summ
  attr(result, "nodes") <- xdim * ydim
  attr(result, "rlen") <- rlen
  attr(result, "Distance") <- distance
  attr(result, "LearnRate") <- alpha
  
  class(result) <- c("SOMiterator", "list")
  
  return(result)
}
