#' Plot Confusion Matrix as Heatmap
#'
#' Creates a heatmap visualization of a confusion matrix with counts displayed
#' in each cell.
#'
#' @param confusion_matrix A confusion matrix (table object), typically from
#'   \code{\link{create_confusion_matrix}}.
#' @param title Character string. Title for the plot. Default is "Confusion Matrix".
#'
#' @return A ggplot2 object representing the confusion matrix heatmap.
#'
#' @details
#' The heatmap uses a blue color gradient where:
#' \itemize{
#'   \item Light blue: Low counts
#'   \item Dark blue: High counts
#'   \item Cell values are displayed in bold black text
#' }
#'
#' The y-axis is reversed so that class 1 appears at the top, matching the
#' conventional confusion matrix layout.
#'
#' @examples
#' \dontrun{
#' # Create and plot confusion matrix
#' cm <- create_confusion_matrix(actual, predicted)
#' p <- plot_confusion_matrix(cm, "Alpine Danger Levels")
#' print(p)
#'
#' # Save plot
#' ggsave("confusion_matrix.png", p, width = 6, height = 5, dpi = 300)
#' }
#'
#' @seealso
#' \code{\link{plot_confusion_matrix_detailed}}, \code{\link{plot_all_confusion_matrices}}
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient scale_y_discrete labs theme_minimal theme element_text element_blank coord_fixed
#' @export
plot_confusion_matrix <- function(confusion_matrix, title = "Confusion Matrix") {
  # Convert confusion matrix to long format for ggplot
  cm_df <- as.data.frame(as.table(confusion_matrix))
  colnames(cm_df) <- c("True", "Predicted", "Count")
  
  # Create the heatmap
  p <- ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = True, fill = Count)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Count), color = "black", size = 5, fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#E8F4F8", high = "#08519C",
                        name = "Count",
                        limits = c(0, max(cm_df$Count))) +
    ggplot2::scale_y_discrete(limits = rev(levels(cm_df$True))) +  # Reverse y-axis so 1 is at top
    ggplot2::labs(title = title,
         x = "Predicted",
         y = "True") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed()
  
  return(p)
}

#' Plot All Three Confusion Matrices
#'
#' Creates heatmap visualizations for all three elevation band confusion matrices
#' and combines them into a single plot.
#'
#' @param results A results object from \code{\link{evaluate_som_prediction}} or
#'   \code{\link{evaluate_som_prediction_presplit}} containing a
#'   \code{confusion_matrices} element with \code{alp}, \code{tl}, and \code{btl}.
#' @param save_individually Logical. If TRUE, saves individual plots as PNG files.
#'   Default is FALSE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{alp}{ggplot object for alpine confusion matrix}
#'     \item{tl}{ggplot object for treeline confusion matrix}
#'     \item{btl}{ggplot object for below treeline confusion matrix}
#'     \item{combined}{Combined plot with all three matrices}
#'   }
#'
#' @details
#' If \code{save_individually = TRUE}, the function saves three PNG files:
#' \itemize{
#'   \item confusion_matrix_alp.png
#'   \item confusion_matrix_tl.png
#'   \item confusion_matrix_btl.png
#' }
#'
#' @examples
#' \dontrun{
#' # After evaluating SOM
#' results <- evaluate_som_prediction(dataNumeric, dataBinary, dataDanger)
#'
#' # Plot all confusion matrices
#' plots <- plot_all_confusion_matrices(results)
#' print(plots$combined)
#'
#' # Save individual plots
#' plots <- plot_all_confusion_matrices(results, save_individually = TRUE)
#'
#' # Save combined plot
#' ggsave("all_cm.png", plots$combined, width = 18, height = 5, dpi = 300)
#' }
#'
#' @seealso \code{\link{plot_confusion_matrix}}
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggsave
#' @export
plot_all_confusion_matrices <- function(results, save_individually = FALSE) {
  # Create individual plots
  p_alp <- plot_confusion_matrix(results$confusion_matrices$alp,
                                 "Confusion Matrix — alp.used")
  
  p_tl <- plot_confusion_matrix(results$confusion_matrices$tl,
                                "Confusion Matrix — tl.used")
  
  p_btl <- plot_confusion_matrix(results$confusion_matrices$btl,
                                 "Confusion Matrix — btl.used")
  
  # Save individually if requested
  if (save_individually) {
    ggplot2::ggsave("confusion_matrix_alp.png", p_alp, width = 6, height = 5, dpi = 300)
    ggplot2::ggsave("confusion_matrix_tl.png", p_tl, width = 6, height = 5, dpi = 300)
    ggplot2::ggsave("confusion_matrix_btl.png", p_btl, width = 6, height = 5, dpi = 300)
    cat("Individual confusion matrix plots saved!\n")
  }
  
  # Combine all three plots
  combined <- gridExtra::grid.arrange(p_alp, p_tl, p_btl, ncol = 3)
  
  return(list(
    alp = p_alp,
    tl = p_tl,
    btl = p_btl,
    combined = combined
  ))
}

#' Plot Detailed Confusion Matrix with Percentages
#'
#' Creates a heatmap visualization of a confusion matrix with both counts and
#' percentages displayed in each cell.
#'
#' @param confusion_matrix A confusion matrix (table object), typically from
#'   \code{\link{create_confusion_matrix}}.
#' @param title Character string. Title for the plot. Default is "Confusion Matrix".
#'
#' @return A ggplot2 object representing the detailed confusion matrix heatmap.
#'
#' @details
#' This function extends \code{\link{plot_confusion_matrix}} by adding percentage
#' information to each cell. Percentages are calculated as row percentages
#' (i.e., what percentage of each true class was predicted as each class).
#'
#' Each cell displays:
#' \itemize{
#'   \item Top number: Count
#'   \item Bottom number (in parentheses): Percentage of row
#' }
#'
#' Cells with zero counts display only "0" to avoid clutter.
#'
#' @examples
#' \dontrun{
#' # Create and plot detailed confusion matrix
#' cm <- create_confusion_matrix(actual, predicted)
#' p <- plot_confusion_matrix_detailed(cm, "Detailed Alpine CM")
#' print(p)
#' }
#'
#' @seealso \code{\link{plot_confusion_matrix}}
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient scale_y_discrete labs theme_minimal theme element_text element_blank coord_fixed
#' @export
plot_confusion_matrix_detailed <- function(confusion_matrix, title = "Confusion Matrix") {
  # Convert confusion matrix to long format
  cm_df <- as.data.frame(as.table(confusion_matrix))
  colnames(cm_df) <- c("True", "Predicted", "Count")
  
  # Calculate percentages by row (true class)
  cm_df$Percentage <- ave(cm_df$Count, cm_df$True, FUN = function(x) x / sum(x) * 100)
  
  # Create label with both count and percentage
  cm_df$Label <- sprintf("%d\n(%.1f%%)", cm_df$Count, cm_df$Percentage)
  cm_df$Label[cm_df$Count == 0] <- "0"
  
  # Create the heatmap
  p <- ggplot2::ggplot(cm_df, ggplot2::aes(x = Predicted, y = True, fill = Count)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Label), color = "black", size = 4, fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#E8F4F8", high = "#08519C",
                        name = "Count",
                        limits = c(0, max(cm_df$Count))) +
    ggplot2::scale_y_discrete(limits = rev(levels(cm_df$True))) +
    ggplot2::labs(title = title,
         x = "Predicted",
         y = "True") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed()
  
  return(p)
}
