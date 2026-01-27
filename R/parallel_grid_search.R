#' Parallel Grid Search for SOM Models
#'
#' Searches across multiple grid sizes in parallel to find the optimal SOM dimensions.
#' Automatically detects platform (Windows vs Unix/Mac) and uses appropriate parallel backend.
#'
#' @param data_list List containing numeric, binary, danger, danger_prev matrices
#' @param dates Date vector for season-based splitting
#' @param test_seasons Character vector of test seasons (e.g., c("2022/2023", "2023/2024"))
#' @param grid_sizes Integer vector of grid sizes to test (default: 10:20)
#' @param n_iterations Number of iterations per grid size (default: 10)
#' @param rlen Number of training iterations for SOM (default: 2000)
#' @param selection_metric Metric to optimize: "weighted_f1", "accuracy", or "mae" (default: "weighted_f1")
#' @param n_cores Number of cores to use (NULL = auto-detect, use all but 1)
#' @param save_dir Directory to save results (default: "./results")
#' @param verbose Print progress messages? (default: TRUE)
#'
#' @return List with:
#'   \itemize{
#'     \item results: List of all model results
#'     \item summary: Data frame summarizing all models
#'     \item best_grid_size: The optimal grid size
#'     \item best_model: The best model object
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- parallel_som_grid_search(
#'   data_list = data_list,
#'   dates = dates,
#'   test_seasons = c("2022/2023", "2023/2024")
#' )
#'
#' # Custom grid sizes and cores
#' results <- parallel_som_grid_search(
#'   data_list = data_list,
#'   dates = dates,
#'   test_seasons = test_seasons,
#'   grid_sizes = c(8, 10, 12, 15, 18, 20),
#'   n_cores = 4
#' )
#'
#' # Access best model
#' best_model <- results$best_model
#' best_size <- results$best_grid_size
#' }
#'
#' @export
parallel_som_grid_search <- function(data_list, 
                                     dates, 
                                     test_seasons,
                                     grid_sizes = 10:20,
                                     n_iterations = 10,
                                     rlen = 2000,
                                     selection_metric = "weighted_f1",
                                     n_cores = NULL,
                                     save_dir = "./results",
                                     verbose = TRUE) {
  
  library(parallel)
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  # Create results directory
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("PARALLEL SOM GRID SEARCH\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("Grid sizes:", paste(grid_sizes, collapse = ", "), "\n")
    cat("Test seasons:", paste(test_seasons, collapse = ", "), "\n")
    cat("Iterations per size:", n_iterations, "\n")
    cat("Selection metric:", selection_metric, "\n")
    cat("Using", n_cores, "cores\n")
    cat("Platform:", .Platform$OS.type, "\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\n")
  }
  
  # Start timer
  start_time <- Sys.time()
  
  # Platform-specific parallel execution
  if (.Platform$OS.type == "unix") {
    # Mac/Linux: Use mclapply (faster, memory efficient)
    if (verbose) cat("Using mclapply (Unix/Mac) for parallel processing\n\n")
    
    results <- mclapply(grid_sizes, function(grid_size) {
      
      if (verbose) {
        cat("Processing grid size:", grid_size, "x", grid_size, "\n")
      }
      
      best_som <- find_best_som_seasonal(
        data_list = data_list,
        date_column = dates,
        test_seasons = test_seasons,
        xdim = grid_size,
        ydim = grid_size,
        n_iterations = n_iterations,
        rlen = rlen,
        selection_metric = selection_metric,
        verbose = FALSE  # Suppress individual model output
      )
      
      # Save
      filename <- file.path(save_dir, paste0('best_som_', grid_size, '.RData'))
      save(best_som, file = filename)
      
      if (verbose) {
        cat("Saved:", filename, "\n")
        cat("  F1:", round(best_som$best_model$metrics$weighted_f1, 4), "\n")
        cat("  Accuracy:", round(best_som$best_model$metrics$accuracy * 100, 2), "%\n\n")
      }
      
      list(grid_size = grid_size, model = best_som)
      
    }, mc.cores = n_cores)
    
  } else {
    # Windows: Use parLapply
    if (verbose) cat("Using parLapply (Windows) for parallel processing\n\n")
    
    cl <- makeCluster(n_cores)
    
    # Export necessary objects to cluster
    clusterExport(cl, 
                 c("data_list", "dates", "test_seasons", "find_best_som_seasonal",
                   "n_iterations", "rlen", "selection_metric", "save_dir", "verbose"),
                 envir = environment())
    
    # Load required packages on each worker
    clusterEvalQ(cl, {
      library(kohonen)
      # Add any other required packages here
    })
    
    results <- parLapply(cl, grid_sizes, function(grid_size) {
      
      if (verbose) {
        cat("Processing grid size:", grid_size, "x", grid_size, "\n")
      }
      
      best_som <- find_best_som_seasonal(
        data_list = data_list,
        date_column = dates,
        test_seasons = test_seasons,
        xdim = grid_size,
        ydim = grid_size,
        n_iterations = n_iterations,
        rlen = rlen,
        selection_metric = selection_metric,
        verbose = FALSE
      )
      
      # Save
      filename <- file.path(save_dir, paste0('best_som_', grid_size, '.RData'))
      save(best_som, file = filename)
      
      list(grid_size = grid_size, model = best_som)
    })
    
    stopCluster(cl)
  }
  
  # End timer
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Summarize results
  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("GRID SEARCH COMPLETE\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("Total time:", round(elapsed, 2), "minutes\n")
    cat("Time per grid:", round(elapsed / length(grid_sizes), 2), "minutes\n\n")
  }
  
  summary_df <- data.frame(
    grid_size = sapply(results, function(x) x$grid_size),
    accuracy = sapply(results, function(x) x$model$best_model$metrics$accuracy),
    weighted_f1 = sapply(results, function(x) x$model$best_model$metrics$weighted_f1),
    mae = sapply(results, function(x) x$model$best_model$metrics$mae),
    iteration = sapply(results, function(x) x$model$best_model$iteration)
  )
  
  # Sort by selection metric
  if (selection_metric == "weighted_f1") {
    summary_df <- summary_df[order(summary_df$weighted_f1, decreasing = TRUE), ]
  } else if (selection_metric == "accuracy") {
    summary_df <- summary_df[order(summary_df$accuracy, decreasing = TRUE), ]
  } else if (selection_metric == "mae") {
    summary_df <- summary_df[order(summary_df$mae, decreasing = FALSE), ]
  }
  
  if (verbose) {
    cat("Results (sorted by", selection_metric, "):\n")
    print(summary_df, row.names = FALSE)
    cat("\n")
  }
  
  # Get best model
  best_idx <- 1  # Already sorted
  best_grid <- summary_df$grid_size[best_idx]
  best_model <- results[[which(sapply(results, function(x) x$grid_size == best_grid))]]$model
  
  if (verbose) {
    cat("BEST MODEL:\n")
    cat("  Grid size:", best_grid, "x", best_grid, "\n")
    cat("  Weighted F1:", round(summary_df$weighted_f1[best_idx], 4), "\n")
    cat("  Accuracy:", round(summary_df$accuracy[best_idx] * 100, 2), "%\n")
    cat("  MAE:", round(summary_df$mae[best_idx], 3), "\n")
    cat("  Best iteration:", summary_df$iteration[best_idx], "\n")
    cat("\n")
    cat("All results saved to:", save_dir, "\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("\n")
  }
  
  return(list(
    results = results,
    summary = summary_df,
    best_grid_size = best_grid,
    best_model = best_model,
    elapsed_time = elapsed
  ))
}


#' Quick Parallel Grid Search (Simplified)
#'
#' Simplified version for quick testing with reasonable defaults
#'
#' @param data_list Data list
#' @param dates Date vector
#' @param test_seasons Test seasons
#' @param max_cores Maximum cores to use (default: all but 1)
#'
#' @export
quick_parallel_search <- function(data_list, dates, test_seasons, max_cores = NULL) {
  
  parallel_som_grid_search(
    data_list = data_list,
    dates = dates,
    test_seasons = test_seasons,
    grid_sizes = seq(10, 20, by = 2),  # 10, 12, 14, 16, 18, 20 (faster)
    n_iterations = 5,                   # Fewer iterations for speed
    rlen = 1000,                        # Shorter training
    n_cores = max_cores,
    verbose = TRUE
  )
}


#' Load and Compare All Grid Search Results
#'
#' Loads all saved models and creates comparison plots
#'
#' @param save_dir Directory containing saved models
#' @param plot Create comparison plots? (default: TRUE)
#'
#' @return Data frame with summary of all models
#'
#' @export
load_grid_search_results <- function(save_dir = "./results", plot = TRUE) {
  
  # Find all saved models
  files <- list.files(save_dir, pattern = "best_som_.*\\.RData", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No saved models found in ", save_dir)
  }
  
  cat("Found", length(files), "saved models\n")
  
  # Load all models
  summary_list <- list()
  
  for (file in files) {
    load(file)  # Loads 'best_som'
    
    # Extract grid size from filename
    grid_size <- as.numeric(gsub(".*best_som_(\\d+)\\.RData", "\\1", file))
    
    summary_list[[length(summary_list) + 1]] <- data.frame(
      grid_size = grid_size,
      accuracy = best_som$best_model$metrics$accuracy,
      weighted_f1 = best_som$best_model$metrics$weighted_f1,
      mae = best_som$best_model$metrics$mae,
      iteration = best_som$best_model$iteration
    )
  }
  
  summary_df <- do.call(rbind, summary_list)
  summary_df <- summary_df[order(summary_df$grid_size), ]
  
  cat("\nSummary:\n")
  print(summary_df, row.names = FALSE)
  
  if (plot) {
    par(mfrow = c(2, 2))
    
    # Weighted F1 vs grid size
    plot(summary_df$grid_size, summary_df$weighted_f1,
         type = "b", pch = 19, col = "steelblue",
         main = "Weighted F1 vs Grid Size",
         xlab = "Grid Size", ylab = "Weighted F1")
    grid()
    best_idx <- which.max(summary_df$weighted_f1)
    points(summary_df$grid_size[best_idx], summary_df$weighted_f1[best_idx],
           col = "red", pch = 19, cex = 2)
    
    # Accuracy vs grid size
    plot(summary_df$grid_size, summary_df$accuracy * 100,
         type = "b", pch = 19, col = "darkgreen",
         main = "Accuracy vs Grid Size",
         xlab = "Grid Size", ylab = "Accuracy (%)")
    grid()
    
    # MAE vs grid size
    plot(summary_df$grid_size, summary_df$mae,
         type = "b", pch = 19, col = "darkred",
         main = "MAE vs Grid Size",
         xlab = "Grid Size", ylab = "MAE")
    grid()
    
    # Best iteration vs grid size
    plot(summary_df$grid_size, summary_df$iteration,
         type = "b", pch = 19, col = "purple",
         main = "Best Iteration vs Grid Size",
         xlab = "Grid Size", ylab = "Iteration")
    grid()
    
    par(mfrow = c(1, 1))
  }
  
  return(summary_df)
}
