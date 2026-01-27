#' Plot Avalanche Problem Types on Hexagonal SOM Grid
#'
#' Visualizes the most common avalanche problem types (P1, P2, P3) for each
#' SOM node on a hexagonal grid.
#'
#' @param SOMit A trained SOM model object (from kohonen package).
#' @param dataBinary A data frame with binary problem type indicators.
#'   Must contain columns like \code{p1.SS}, \code{p1.WdS}, etc.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param show_labels Logical. If TRUE, shows problem type labels. Default is TRUE.
#'
#' @return A ggplot object showing the hexagonal grid with problem type labels.
#'
#' @details
#' \strong{Problem Type Display:}
#' For each node, shows the most frequent problem type for:
#' \itemize{
#'   \item P1 (Primary problem) - Bold, larger font
#'   \item P2 (Secondary problem) - Normal font
#'   \item P3 (Tertiary problem) - Gray, smaller font
#' }
#'
#' \strong{Problem Type Codes:}
#' Common codes include SS (Storm Slab), WdS (Wind Slab), PS (Persistent Slab),
#' WL (Wet Loose), C (Cornice), etc.
#'
#' \strong{Special Cases:}
#' \itemize{
#'   \item "none" - No problem identified
#'   \item "—" - Empty node
#'   \item If all observations in a node have all three problems as "none",
#'     displays "none" for all three
#' }
#'
#' @examples
#' \dontrun{
#' # Train SOM
#' som_model <- supersom(data_list, grid = somgrid(15, 11, "hexagonal"), rlen = 1000)
#'
#' # Plot problem types
#' p <- plot_problems_hex(som_model, dataBinary, xdim = 15, ydim = 11)
#' print(p)
#'
#' # Without labels
#' p <- plot_problems_hex(som_model, dataBinary, show_labels = FALSE)
#' print(p)
#'
#' # Save
#' ggsave("som_problems_hex.png", p, width = 18, height = 12, dpi = 300)
#' }
#'
#' @seealso \code{\link{plot_danger_hex}}
#'
#' @importFrom ggplot2 ggplot geom_polygon annotate theme_void coord_equal theme margin
#' @export
plot_problems_hex <- function(SOMit, dataBinary, xdim = 15, ydim = 11, show_labels = TRUE) {
  
  # Convert matrix to data frame if necessary
  if (is.matrix(dataBinary)) {
    dataBinary <- as.data.frame(dataBinary)
  }
  
  # Get node assignments
  node_assignments <- SOMit$unit.classif
  
  # Get problem columns
  p1_cols <- grep("^p1\\.", colnames(dataBinary), value = TRUE)
  p2_cols <- grep("^p2\\.", colnames(dataBinary), value = TRUE)
  p3_cols <- grep("^p3\\.", colnames(dataBinary), value = TRUE)
  
  # Get hexagonal positions
  hex_pos <- get_hex_positions(xdim, ydim)
  
  # Create the plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))
  
  # For each node
  for (i in 1:(xdim * ydim)) {
    node_indices <- which(node_assignments == i)
    
    hex_data <- create_hexagon(hex_pos$x[i], hex_pos$y[i])
    
    # Add hexagon border
    p <- p + ggplot2::geom_polygon(data = hex_data,
                          ggplot2::aes(x = x, y = y),
                          fill = "white",
                          color = "gray30",
                          linewidth = 0.3)
    
    if (length(node_indices) > 0) {
      # Get binary data for this node
      node_binary <- dataBinary[node_indices, , drop = FALSE]
      
      # Identify rows where p1, p2, AND p3 are all 'none'
      p1_none <- node_binary[, "p1.none"] == 1
      p2_none <- node_binary[, "p2.none"] == 1
      p3_none <- node_binary[, "p3.none"] == 1
      all_none_rows <- p1_none & p2_none & p3_none
      
      if (all(all_none_rows)) {
        p1_problem <- "none"
        p2_problem <- "none"
        p3_problem <- "none"
      } else {
        node_binary_filtered <- node_binary[!all_none_rows, , drop = FALSE]
        
        # P1
        node_binary_p1 <- node_binary_filtered[, p1_cols, drop = FALSE]
        col_sums_p1 <- colSums(node_binary_p1, na.rm = TRUE)
        p1_problem <- if (max(col_sums_p1) == 0) "—" else sub("^p1\\.", "", names(which.max(col_sums_p1)))
        
        # P2
        node_binary_p2 <- node_binary_filtered[, p2_cols, drop = FALSE]
        col_sums_p2 <- colSums(node_binary_p2, na.rm = TRUE)
        p2_problem <- if (max(col_sums_p2) == 0) "—" else sub("^p2\\.", "", names(which.max(col_sums_p2)))
        
        # P3
        node_binary_p3 <- node_binary_filtered[, p3_cols, drop = FALSE]
        col_sums_p3 <- colSums(node_binary_p3, na.rm = TRUE)
        p3_problem <- if (max(col_sums_p3) == 0) "—" else sub("^p3\\.", "", names(which.max(col_sums_p3)))
      }
      
      # Add text labels (if requested)
      if (show_labels) {
        p <- p + ggplot2::annotate("text",
                          x = hex_pos$x[i],
                          y = hex_pos$y[i] + 0.2,
                          label = paste0("P1: ", p1_problem),
                          size = 3,
                          fontface = "bold",
                          hjust = 0.5)
        
        p <- p + ggplot2::annotate("text",
                          x = hex_pos$x[i],
                          y = hex_pos$y[i],
                          label = paste0("P2: ", p2_problem),
                          size = 2.5,
                          hjust = 0.5)
        
        p <- p + ggplot2::annotate("text",
                          x = hex_pos$x[i],
                          y = hex_pos$y[i] - 0.2,
                          label = paste0("P3: ", p3_problem),
                          size = 2.5,
                          color = "gray40",
                          hjust = 0.5)
      }
      
    } else {
      if (show_labels) {
        p <- p + ggplot2::annotate("text",
                          x = hex_pos$x[i],
                          y = hex_pos$y[i],
                          label = "—",
                          size = 6,
                          color = "gray70",
                          hjust = 0.5)
      }
    }
  }
  
  p <- p + ggplot2::labs(title = "SOM Problem Types Distribution")
  
  return(p)
}
