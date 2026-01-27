#' Create Scree Plots for SOM Grid Size Comparison
#'
#' Analyzes performance across different SOM grid sizes and creates scree plots
#' to help identify the optimal grid dimensions. Loads saved model results and
#' visualizes accuracy and F1 scores vs grid size.
#'
#' @param grid_sizes Integer vector. Grid sizes to analyze. Default is 10:20.
#' @param results_path Character. Path to directory containing saved model files.
#'   Default is './results/'.
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{Data frame with performance metrics for each grid size:
#'       \itemize{
#'         \item grid_size: Grid dimension (N for N×N grid)
#'         \item total_nodes: Total number of nodes (N²)
#'         \item alp_accuracy, tl_accuracy, btl_accuracy: Accuracy by elevation
#'         \item mean_accuracy: Average accuracy across elevations
#'         \item alp_weighted_f1, tl_weighted_f1, btl_weighted_f1: F1 by elevation
#'         \item mean_weighted_f1: Average weighted F1 across elevations
#'         \item best_seed: Random seed of best model
#'         \item improvement_acc: Marginal improvement in accuracy
#'         \item improvement_f1: Marginal improvement in F1
#'       }
#'     }
#'     \item{plots}{List of ggplot objects:
#'       \itemize{
#'         \item accuracy_scree: Mean accuracy vs grid size
#'         \item f1_scree: Mean weighted F1 vs grid size
#'         \item accuracy_vs_nodes: Mean accuracy vs total nodes
#'         \item by_elevation: Accuracy by grid size for all elevation bands
#'         \item marginal_improvement: Rate of improvement in F1
#'       }
#'     }
#'     \item{best_by_accuracy}{Grid size with highest mean accuracy}
#'     \item{best_by_f1}{Grid size with highest mean weighted F1}
#'   }
#'
#' @details
#' \strong{Scree Plot Analysis:}
#' A scree plot helps identify the "elbow point" where increasing grid size
#' provides diminishing returns. The optimal grid size typically occurs at the
#' elbow - where the curve starts to flatten.
#'
#' \strong{Required File Structure:}
#' The function expects saved model files named:
#' \code{best_som_<size>_<size>} in the \code{results_path} directory.
#' These files should contain \code{best_som} objects from
#' \code{\link{find_best_som}} or \code{\link{find_best_som_seasonal}}.
#'
#' \strong{Generated Plots:}
#' \enumerate{
#'   \item \strong{Accuracy Scree}: Shows how mean accuracy changes with grid size
#'   \item \strong{F1 Scree}: Shows how mean weighted F1 changes with grid size
#'   \item \strong{Accuracy vs Nodes}: Plots accuracy against total node count
#'   \item \strong{By Elevation}: Separate lines for ALP, TL, BTL accuracies
#'   \item \strong{Marginal Improvement}: Rate of change in F1 (helps find elbow)
#' }
#'
#' \strong{Elbow Detection:}
#' The function automatically identifies a suggested elbow point by finding
#' where the rate of improvement starts to decrease most rapidly.
#'
#' @examples
#' \dontrun{
#' # After running grid size iterations with find_best_som
#' # (assumes you've saved models for grid sizes 10x10 through 20x20)
#'
#' # Create scree plots
#' scree_results <- create_som_scree_plot(
#'   grid_sizes = 10:20,
#'   results_path = './results/'
#' )
#'
#' # View accuracy scree plot
#' print(scree_results$plots$accuracy_scree)
#'
#' # View F1 scree plot
#' print(scree_results$plots$f1_scree)
#'
#' # View marginal improvement (for elbow detection)
#' print(scree_results$plots$marginal_improvement)
#'
#' # Get recommended grid sizes
#' cat("Best grid size by accuracy:", scree_results$best_by_accuracy, "\n")
#' cat("Best grid size by F1:", scree_results$best_by_f1, "\n")
#'
#' # View all elevation bands
#' print(scree_results$plots$by_elevation)
#'
#' # Access performance data
#' print(scree_results$data)
#' }
#'
#' @seealso
#' \code{\link{save_scree_plots}}, \code{\link{find_best_som}},
#' \code{\link{find_best_som_seasonal}}
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text geom_vline geom_hline labs theme_minimal theme element_text scale_x_continuous scale_color_manual
#' @importFrom tidyr pivot_longer
#' @export
create_som_scree_plot <- function(grid_sizes = 10:20, results_path = './results/') {
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("CREATING SCREE PLOT FOR SOM GRID SIZES\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\nLoading results from:", results_path, "\n")
  
  # Collect data from saved models
  scree_data <- data.frame(
    grid_size = integer(),
    total_nodes = integer(),
    alp_accuracy = numeric(),
    tl_accuracy = numeric(),
    btl_accuracy = numeric(),
    mean_accuracy = numeric(),
    alp_weighted_f1 = numeric(),
    tl_weighted_f1 = numeric(),
    btl_weighted_f1 = numeric(),
    mean_weighted_f1 = numeric(),
    best_seed = integer()
  )
  
  for (grid_size in grid_sizes) {
    file_name <- paste0(results_path, 'best_som_', grid_size, '_', grid_size)
    
    if (file.exists(file_name)) {
      load(file_name)
      
      # Extract metrics
      alp_acc <- best_som$best_model$metrics$alp$accuracy
      tl_acc <- best_som$best_model$metrics$tl$accuracy
      btl_acc <- best_som$best_model$metrics$btl$accuracy
      
      # Get weighted F1 (last row of metrics)
      alp_wf1 <- best_som$best_model$metrics$alp$metrics$F1[nrow(best_som$best_model$metrics$alp$metrics)]
      tl_wf1 <- best_som$best_model$metrics$tl$metrics$F1[nrow(best_som$best_model$metrics$tl$metrics)]
      btl_wf1 <- best_som$best_model$metrics$btl$metrics$F1[nrow(best_som$best_model$metrics$btl$metrics)]
      
      scree_data <- rbind(scree_data, data.frame(
        grid_size = grid_size,
        total_nodes = grid_size * grid_size,
        alp_accuracy = alp_acc,
        tl_accuracy = tl_acc,
        btl_accuracy = btl_acc,
        mean_accuracy = mean(c(alp_acc, tl_acc, btl_acc)),
        alp_weighted_f1 = alp_wf1,
        tl_weighted_f1 = tl_wf1,
        btl_weighted_f1 = btl_wf1,
        mean_weighted_f1 = mean(c(alp_wf1, tl_wf1, btl_wf1)),
        best_seed = best_som$best_seed
      ))
      
      cat("  Loaded:", grid_size, "x", grid_size, "(", grid_size^2, "nodes )\n")
    } else {
      cat("  WARNING: File not found -", file_name, "\n")
    }
  }
  
  if (nrow(scree_data) == 0) {
    stop("No data loaded! Check that files exist in the specified path.")
  }
  
  cat("\nLoaded", nrow(scree_data), "grid sizes\n")
  
  # Calculate rate of improvement (marginal gain)
  scree_data <- scree_data[order(scree_data$grid_size), ]
  scree_data$improvement_acc <- c(NA, diff(scree_data$mean_accuracy))
  scree_data$improvement_f1 <- c(NA, diff(scree_data$mean_weighted_f1))
  
  # Print summary
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("GRID SIZE PERFORMANCE SUMMARY\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  print(scree_data[, c("grid_size", "total_nodes", "mean_accuracy", "mean_weighted_f1")], 
        row.names = FALSE)
  
  # Create plots
  
  # Plot 1: Accuracy vs Grid Size
  p1 <- ggplot2::ggplot(scree_data, ggplot2::aes(x = grid_size, y = mean_accuracy)) +
    ggplot2::geom_line(color = "#08519C", linewidth = 1.2) +
    ggplot2::geom_point(color = "#08519C", size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(grid_size, "×", grid_size)), 
              vjust = -1, size = 3) +
    ggplot2::labs(title = "Scree Plot: Mean Accuracy by Grid Size",
         x = "Grid Dimension (N × N)",
         y = "Mean Accuracy",
         subtitle = "Average across ALP, TL, and BTL") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::scale_x_continuous(breaks = scree_data$grid_size)
  
  # Plot 2: Weighted F1 vs Grid Size
  p2 <- ggplot2::ggplot(scree_data, ggplot2::aes(x = grid_size, y = mean_weighted_f1)) +
    ggplot2::geom_line(color = "#006D2C", linewidth = 1.2) +
    ggplot2::geom_point(color = "#006D2C", size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(grid_size, "×", grid_size)), 
              vjust = -1, size = 3) +
    ggplot2::labs(title = "Scree Plot: Mean Weighted F1 by Grid Size",
         x = "Grid Dimension (N × N)",
         y = "Mean Weighted F1",
         subtitle = "Average across ALP, TL, and BTL") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::scale_x_continuous(breaks = scree_data$grid_size)
  
  # Plot 3: Accuracy vs Total Nodes
  p3 <- ggplot2::ggplot(scree_data, ggplot2::aes(x = total_nodes, y = mean_accuracy)) +
    ggplot2::geom_line(color = "#08519C", linewidth = 1.2) +
    ggplot2::geom_point(color = "#08519C", size = 3) +
    ggplot2::geom_vline(xintercept = scree_data$total_nodes[which.max(scree_data$mean_accuracy)],
               linetype = "dashed", color = "red", alpha = 0.5) +
    ggplot2::labs(title = "Mean Accuracy vs Total Number of Nodes",
         x = "Total Nodes (N²)",
         y = "Mean Accuracy") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold")
    )
  
  # Plot 4: All three elevation bands on one plot
  scree_long_acc <- tidyr::pivot_longer(scree_data, 
                                 cols = c(alp_accuracy, tl_accuracy, btl_accuracy),
                                 names_to = "band",
                                 values_to = "accuracy")
  scree_long_acc$band <- gsub("_accuracy", "", scree_long_acc$band)
  scree_long_acc$band <- toupper(scree_long_acc$band)
  
  p4 <- ggplot2::ggplot(scree_long_acc, ggplot2::aes(x = grid_size, y = accuracy, color = band, group = band)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(title = "Accuracy by Grid Size - All Elevation Bands",
         x = "Grid Dimension (N × N)",
         y = "Accuracy",
         color = "Elevation Band") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::scale_x_continuous(breaks = scree_data$grid_size) +
    ggplot2::scale_color_manual(values = c("ALP" = "#08519C", "TL" = "#006D2C", "BTL" = "#A63603"))
  
  # Plot 5: Marginal improvement (elbow detection)
  scree_improvement <- scree_data[!is.na(scree_data$improvement_f1), ]
  
  p5 <- ggplot2::ggplot(scree_improvement, ggplot2::aes(x = grid_size, y = improvement_f1)) +
    ggplot2::geom_line(color = "#8B0000", linewidth = 1.2) +
    ggplot2::geom_point(color = "#8B0000", size = 3) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(title = "Marginal Improvement in Weighted F1",
         x = "Grid Dimension (N × N)",
         y = "Change in Mean Weighted F1",
         subtitle = "Rate of improvement vs. previous grid size") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::scale_x_continuous(breaks = scree_data$grid_size)
  
  # Find elbow point (largest decrease in improvement rate)
  if (nrow(scree_improvement) > 2) {
    second_diff <- diff(scree_improvement$improvement_f1)
    elbow_idx <- which.min(second_diff) + 1  # +1 because diff reduces length
    elbow_size <- scree_improvement$grid_size[elbow_idx]
    
    cat("\nSuggested 'elbow' point:", elbow_size, "×", elbow_size, "\n")
    cat("(Grid size where improvement rate starts to diminish)\n")
  }
  
  # Identify best performing grid size
  best_idx_acc <- which.max(scree_data$mean_accuracy)
  best_idx_f1 <- which.max(scree_data$mean_weighted_f1)
  
  cat("\nBest performing grid sizes:\n")
  cat("  By Mean Accuracy  :", scree_data$grid_size[best_idx_acc], "×", 
      scree_data$grid_size[best_idx_acc], 
      "(Accuracy =", round(scree_data$mean_accuracy[best_idx_acc], 4), ")\n")
  cat("  By Weighted F1    :", scree_data$grid_size[best_idx_f1], "×", 
      scree_data$grid_size[best_idx_f1],
      "(F1 =", round(scree_data$mean_weighted_f1[best_idx_f1], 4), ")\n")
  
  return(list(
    data = scree_data,
    plots = list(
      accuracy_scree = p1,
      f1_scree = p2,
      accuracy_vs_nodes = p3,
      by_elevation = p4,
      marginal_improvement = p5
    ),
    best_by_accuracy = scree_data$grid_size[best_idx_acc],
    best_by_f1 = scree_data$grid_size[best_idx_f1]
  ))
}
