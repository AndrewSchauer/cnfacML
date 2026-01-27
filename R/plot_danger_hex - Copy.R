#' Plot Danger Level Histograms on Hexagonal SOM Grid by Elevation Band
#'
#' Visualizes danger level distributions for each SOM node as histograms
#' on a hexagonal grid. Creates separate plots for Alpine, Treeline, and
#' Below Treeline elevation bands.
#'
#' @param SOMit A trained SOM model object (from kohonen package).
#' @param dataDanger A data frame with danger levels containing columns:
#'   \code{alp.used}, \code{tl.used}, \code{btl.used}.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param show_labels Logical. If TRUE, shows sample counts and "empty" labels.
#'   Default is TRUE.
#'
#' @return A list with three ggplot objects:
#'   \describe{
#'     \item{alp}{Alpine elevation band danger distribution}
#'     \item{tl}{Treeline elevation band danger distribution}
#'     \item{btl}{Below treeline elevation band danger distribution}
#'   }
#'
#' @details
#' \strong{Visualization Components:}
#' \itemize{
#'   \item Each hexagon represents one SOM node
#'   \item Bars within hexagons show danger level distribution for that elevation
#'   \item Bar colors match danger levels (green to black)
#'   \item Sample size (n=X) displayed below each histogram
#'   \item Empty nodes labeled as "empty"
#' }
#'
#' \strong{Elevation-Specific Plots:}
#' Unlike the original version that used maximum danger across all elevations,
#' this version creates three separate plots showing the actual danger distribution
#' for each elevation band independently.
#'
#' \strong{Histogram Scaling:}
#' Bar heights are normalized within each hexagon (tallest bar = max height).
#' This allows comparison of distributions within nodes but not between nodes.
#'
#' @examples
#' \dontrun{
#' # Train SOM
#' som_model <- supersom(data_list, grid = somgrid(15, 11, "hexagonal"), rlen = 1000)
#'
#' # Plot danger distributions for all elevation bands
#' plots <- plot_danger_hex(som_model, dataDanger, xdim = 15, ydim = 11)
#'
#' # View individual plots
#' print(plots$alp)   # Alpine
#' print(plots$tl)    # Treeline
#' print(plots$btl)   # Below Treeline
#'
#' # Save all three plots
#' ggsave("som_danger_alp.png", plots$alp, width = 18, height = 12, dpi = 300)
#' ggsave("som_danger_tl.png", plots$tl, width = 18, height = 12, dpi = 300)
#' ggsave("som_danger_btl.png", plots$btl, width = 18, height = 12, dpi = 300)
#'
#' # Or use gridExtra to combine them
#' library(gridExtra)
#' combined <- grid.arrange(plots$alp, plots$tl, plots$btl, ncol = 1)
#' ggsave("som_danger_all_elevations.png", combined, width = 18, height = 36, dpi = 300)
#' }
#'
#' @seealso \code{\link{plot_problems_hex}}
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_rect geom_text scale_fill_identity labs theme_void coord_equal theme
#' @export
plot_danger_hex <- function(SOMit, dataDanger, xdim = 15, ydim = 11, show_labels = TRUE) {
  
  # Convert matrix to data frame if necessary
  if (is.matrix(dataDanger)) {
    dataDanger <- as.data.frame(dataDanger)
  }
  
  # Ensure column names are correct
  if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(dataDanger))) {
    colnames(dataDanger) <- c("alp.used", "tl.used", "btl.used")
  }
  
  # Define danger level colors
  danger_colors <- c("#50B848", "#FFF200", "#F7941E", "#ED1C24", "#231F20")
  
  # Get node assignments
  node_assignments <- SOMit$unit.classif
  
  # Helper function to create plot for one elevation band
  create_elevation_plot <- function(danger_values, elevation_name) {
    
    # Get hexagonal positions
    hex_pos <- get_hex_positions(xdim, ydim)
    
    # Prepare data for all hexagons and bars
    all_hex_data <- data.frame()
    all_bar_data <- data.frame()
    all_label_data <- data.frame()
    
    # Collect all data first
    for (i in 1:(xdim * ydim)) {
      node_dangers <- danger_values[node_assignments == i]
      
      # Hexagon outline
      hex_coords <- create_hexagon(hex_pos$x[i], hex_pos$y[i])
      hex_coords$node <- i
      all_hex_data <- rbind(all_hex_data, hex_coords)
      
      if (length(node_dangers) > 0) {
        # Filter out NA and invalid values (0 or outside 1-5 range)
        valid_dangers <- node_dangers[!is.na(node_dangers) & node_dangers >= 1 & node_dangers <= 5]
        
        if (length(valid_dangers) > 0) {
          # Create histogram
          h <- hist(valid_dangers,
                    breaks = seq(0.5, 5.5, by = 1),
                    plot = FALSE)
          
          # Calculate bar positions within hexagon
          n_bars <- length(h$counts)
          bar_width <- 0.8 / n_bars
          max_count <- max(h$counts)
          
          if (max_count > 0) {
            for (j in 1:n_bars) {
              if (h$counts[j] > 0) {
                bar_height <- (h$counts[j] / max_count) * 0.45
                bar_x_center <- hex_pos$x[i] - 0.4 + (j - 0.5) * bar_width
                bar_y_bottom <- hex_pos$y[i] - 0.25
                bar_y_top <- bar_y_bottom + bar_height
                
                # Get color based on danger level
                danger_level <- round(h$mids[j])
                bar_color <- danger_colors[danger_level]
                
                # Store bar data
                bar_df <- data.frame(
                  node = i,
                  bar_id = paste(i, j, sep = "_"),
                  xmin = bar_x_center - bar_width/2.5,
                  xmax = bar_x_center + bar_width/2.5,
                  ymin = bar_y_bottom,
                  ymax = bar_y_top,
                  fill_color = bar_color
                )
                all_bar_data <- rbind(all_bar_data, bar_df)
              }
            }
          }
          
          # Store label data
          label_df <- data.frame(
            node = i,
            x = hex_pos$x[i],
            y = hex_pos$y[i] - 0.35,
            label = paste0("n=", length(valid_dangers)),
            is_empty = FALSE
          )
          all_label_data <- rbind(all_label_data, label_df)
          
        } else {
          # No valid danger values - treat as empty node
          label_df <- data.frame(
            node = i,
            x = hex_pos$x[i],
            y = hex_pos$y[i],
            label = "empty",
            is_empty = TRUE
          )
          all_label_data <- rbind(all_label_data, label_df)
        }
        
      } else {
        # Empty node label
        label_df <- data.frame(
          node = i,
          x = hex_pos$x[i],
          y = hex_pos$y[i],
          label = "empty",
          is_empty = TRUE
        )
        all_label_data <- rbind(all_label_data, label_df)
      }
    }
    
    # Create the plot
    p <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::coord_equal() +
      ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))
    
    # Add all hexagon borders
    p <- p + ggplot2::geom_polygon(data = all_hex_data,
                          ggplot2::aes(x = x, y = y, group = node),
                          fill = "white",
                          color = "gray30",
                          linewidth = 0.8)
    
    # Add all bars
    if (nrow(all_bar_data) > 0) {
      p <- p + ggplot2::geom_rect(data = all_bar_data,
                         ggplot2::aes(xmin = xmin, xmax = xmax,
                             ymin = ymin, ymax = ymax,
                             fill = fill_color),
                         color = NA)
      
      p <- p + ggplot2::scale_fill_identity()
    }
    
    # Add all labels (if requested)
    if (show_labels) {
      p <- p + ggplot2::geom_text(data = all_label_data[!all_label_data$is_empty, ],
                         ggplot2::aes(x = x, y = y, label = label),
                         size = 2,
                         hjust = 0.5)
      
      p <- p + ggplot2::geom_text(data = all_label_data[all_label_data$is_empty, ],
                         ggplot2::aes(x = x, y = y, label = label),
                         size = 2,
                         color = "gray50",
                         hjust = 0.5)
    }
    
    p <- p + ggplot2::labs(title = paste0("SOM Danger Levels - ", elevation_name))
    
    return(p)
  }
  
  # Create plots for all three elevation bands
  p_alp <- create_elevation_plot(dataDanger$alp.used, "Alpine")
  p_tl <- create_elevation_plot(dataDanger$tl.used, "Treeline")
  p_btl <- create_elevation_plot(dataDanger$btl.used, "Below Treeline")
  
  # Return list of three plots
  return(list(
    alp = p_alp,
    tl = p_tl,
    btl = p_btl
  ))
}
