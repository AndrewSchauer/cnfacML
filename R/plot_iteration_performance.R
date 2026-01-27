#' Plot SOM Performance Across Iterations
#'
#' Creates line plots showing accuracy and weighted F1 scores across multiple
#' SOM training iterations for all three elevation bands.
#'
#' @param best_som_results A results object from \code{\link{find_best_som}} or
#'   \code{\link{find_best_som_seasonal}} containing a \code{performance_summary}
#'   data frame.
#'
#' @return A list containing two ggplot objects:
#'   \describe{
#'     \item{accuracy_plot}{Line plot of accuracy by iteration for ALP, TL, BTL,
#'       and Mean}
#'     \item{f1_plot}{Line plot of weighted F1 scores by iteration for ALP, TL,
#'       BTL, and Mean}
#'   }
#'
#' @details
#' The function creates two visualizations:
#'
#' \strong{Accuracy Plot:}
#' \itemize{
#'   \item Shows accuracy trends across iterations
#'   \item Separate lines for each elevation band (ALP, TL, BTL) and mean
#'   \item Useful for identifying performance stability
#' }
#'
#' \strong{Weighted F1 Plot:}
#' \itemize{
#'   \item Shows weighted F1 score trends across iterations
#'   \item Accounts for class imbalance in each elevation band
#'   \item Better metric for imbalanced datasets
#' }
#'
#' Both plots include:
#' \itemize{
#'   \item Lines connecting iteration points
#'   \item Individual points for each iteration
#'   \item Color-coded by metric type
#'   \item Minimal theme for clarity
#' }
#'
#' @examples
#' \dontrun{
#' # After finding best model
#' best_som <- find_best_som(dataNumeric, dataBinary, dataDanger,
#'                           n_iterations = 10)
#'
#' # Create plots
#' plots <- plot_iteration_performance(best_som)
#'
#' # Display plots
#' print(plots$accuracy_plot)
#' print(plots$f1_plot)
#'
#' # Save plots
#' ggsave("accuracy_iterations.png", plots$accuracy_plot,
#'        width = 8, height = 5, dpi = 300)
#' ggsave("f1_iterations.png", plots$f1_plot,
#'        width = 8, height = 5, dpi = 300)
#'
#' # Display both plots together
#' library(gridExtra)
#' grid.arrange(plots$accuracy_plot, plots$f1_plot, ncol = 2)
#' }
#'
#' @seealso \code{\link{find_best_som}}, \code{\link{find_best_som_seasonal}}
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal theme
#' @importFrom tidyr pivot_longer
#' @export
plot_iteration_performance <- function(best_som_results) {
  
  perf <- best_som_results$performance_summary
  
  # Reshape data for plotting
  acc_data <- perf[, c("iteration", "alp_accuracy", "tl_accuracy", "btl_accuracy", "mean_accuracy")]
  acc_long <- tidyr::pivot_longer(acc_data, cols = -iteration, names_to = "metric", values_to = "accuracy")
  
  f1_data <- perf[, c("iteration", "alp_weighted_f1", "tl_weighted_f1", "btl_weighted_f1", "mean_weighted_f1")]
  f1_long <- tidyr::pivot_longer(f1_data, cols = -iteration, names_to = "metric", values_to = "weighted_f1")
  
  # Clean up names
  acc_long$metric <- gsub("_accuracy", "", acc_long$metric)
  acc_long$metric <- gsub("alp", "ALP", acc_long$metric)
  acc_long$metric <- gsub("tl", "TL", acc_long$metric)
  acc_long$metric <- gsub("btl", "BTL", acc_long$metric)
  acc_long$metric <- gsub("mean", "Mean", acc_long$metric)
  
  f1_long$metric <- gsub("_weighted_f1", "", f1_long$metric)
  f1_long$metric <- gsub("alp", "ALP", f1_long$metric)
  f1_long$metric <- gsub("tl", "TL", f1_long$metric)
  f1_long$metric <- gsub("btl", "BTL", f1_long$metric)
  f1_long$metric <- gsub("mean", "Mean", f1_long$metric)
  
  # Accuracy plot
  p1 <- ggplot2::ggplot(acc_long, ggplot2::aes(x = iteration, y = accuracy, 
                                               color = metric, group = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Accuracy Across Iterations",
         x = "Iteration",
         y = "Accuracy",
         color = "Metric") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  # Weighted F1 plot
  p2 <- ggplot2::ggplot(f1_long, ggplot2::aes(x = iteration, y = weighted_f1, 
                                              color = metric, group = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Weighted F1 Across Iterations",
         x = "Iteration",
         y = "Weighted F1",
         color = "Metric") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  return(list(accuracy_plot = p1, f1_plot = p2))
}
