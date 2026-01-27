#' Plot Danger Level Histograms on Hexagonal SOM Grid by Elevation Band
#'
#' Visualizes danger level distributions for each SOM node as histograms
#' on a hexagonal grid. Creates separate plots for Alpine, Treeline, and
#' Below Treeline elevation bands, plus a codebook triangle visualization.
#'
#' @param SOMit A trained SOM model object (from kohonen package).
#' @param dataDanger A data frame with danger levels containing columns:
#'   \code{alp.used}, \code{tl.used}, \code{btl.used}.
#' @param xdim Integer. Width of the SOM grid. Default is 15.
#' @param ydim Integer. Height of the SOM grid. Default is 11.
#' @param show_labels Logical. If TRUE, shows sample counts and "empty" labels.
#'   Default is TRUE.
#'
#' @return A list with four ggplot objects:
#'   \describe{
#'     \item{alp}{Alpine elevation band danger distribution}
#'     \item{tl}{Treeline elevation band danger distribution}
#'     \item{btl}{Below treeline elevation band danger distribution}
#'     \item{triangle}{Codebook danger triangles showing learned danger patterns}
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
#'   \item Triangle plot shows codebook values as elevation-stacked triangles
#' }
#'
#' \strong{Elevation-Specific Plots:}
#' Unlike the original version that used maximum danger across all elevations,
#' this version creates three separate plots showing the actual danger distribution
#' for each elevation band independently.
#'
#' \strong{Triangle Plot:}
#' The triangle plot displays the learned danger patterns from the SOM codebook.
#' Each node shows a stacked triangle with three bands (top to bottom: Alpine,
#' Treeline, Below Treeline), colored according to the codebook danger values.
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
#' print(plots$alp)       # Alpine
#' print(plots$tl)        # Treeline
#' print(plots$btl)       # Below Treeline
#' print(plots$triangle)  # Codebook triangles
#'
#' # Save all four plots
#' ggsave("som_danger_alp.png", plots$alp, width = 18, height = 12, dpi = 300)
#' ggsave("som_danger_tl.png", plots$tl, width = 18, height = 12, dpi = 300)
#' ggsave("som_danger_btl.png", plots$btl, width = 18, height = 12, dpi = 300)
#' ggsave("som_danger_triangle.png", plots$triangle, width = 18, height = 12, dpi = 300)
#'
#' # Or use gridExtra to combine them
#' library(gridExtra)
#' combined <- grid.arrange(plots$alp, plots$tl, plots$btl, plots$triangle, ncol = 2)
#' ggsave("som_danger_all.png", combined, width = 36, height = 24, dpi = 300)
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

  # Helper function to create triangle plot showing codebook values
  create_triangle_plot <- function() {

    # Get hexagonal positions
    hex_pos <- get_hex_positions(xdim, ydim)

    # Extract codebook danger values
    codebook_danger <- SOMit$codes$danger

    # Prepare data
    all_hex_data <- data.frame()
    all_triangle_data <- data.frame()

    # Collect all data
    for (i in 1:(xdim * ydim)) {
      # Hexagon outline
      hex_coords <- create_hexagon(hex_pos$x[i], hex_pos$y[i])
      hex_coords$node <- i
      all_hex_data <- rbind(all_hex_data, hex_coords)

      # Get codebook values for this node
      alp_val <- codebook_danger[i, "alp.used"]
      tl_val <- codebook_danger[i, "tl.used"]
      btl_val <- codebook_danger[i, "btl.used"]

      # Helper function to get color (gray47 for NA/0, danger color otherwise)
      get_danger_color <- function(val) {
        if (is.na(val) || val < 0.5 || val > 5) {
          return("gray47")
        } else {
          return(danger_colors[round(val)])
        }
      }

      # Get colors for each elevation band
      alp_color <- get_danger_color(alp_val)
      tl_color <- get_danger_color(tl_val)
      btl_color <- get_danger_color(btl_val)

      # Always draw triangles (even with gray sections for missing data)
      if (TRUE) {

        # Define triangle geometry within hexagon
        # Triangle will be centered and sized to fit nicely INSIDE the hexagon
        # For equilateral triangle: height = width * sqrt(3)/2 ≈ width * 0.866
        triangle_width <- 0.5
        triangle_height <- 0.433  # width * 0.866 = 0.5 * 0.866 ≈ 0.433
        center_x <- as.numeric(hex_pos$x[i])
        center_y <- as.numeric(hex_pos$y[i]) + 0.05  # Shift up slightly for better vertical centering

        # Calculate triangle vertices
        # Top vertex (Alpine) - apex of triangle
        top_x <- center_x
        top_y <- center_y + triangle_height/2

        # Bottom left vertex
        bottom_left_x <- center_x - triangle_width/2
        bottom_left_y <- center_y - triangle_height/2

        # Bottom right vertex
        bottom_right_x <- center_x + triangle_width/2
        bottom_right_y <- center_y - triangle_height/2

        # Create three horizontal bands (equal height bands)
        # Triangle is WIDEST at BOTTOM, NARROWEST at TOP
        band_height <- triangle_height / 3

        # Helper function to get width at a given height from bottom
        # At bottom (height 0): width = triangle_width
        # At top (height = triangle_height): width = 0
        get_width_at_height <- function(height_from_bottom) {
          return(triangle_width * (1 - height_from_bottom / triangle_height))
        }

        # Alpine (top third) - small triangular section at apex
        alp_top_y <- top_y  # Apex
        alp_bottom_y <- top_y - band_height
        # At top: point (width = 0)
        # At bottom of alpine (2/3 up from base): width = triangle_width * (1/3)
        alp_bottom_half_width <- get_width_at_height(2 * band_height) / 2
        alp_left_x <- center_x - alp_bottom_half_width
        alp_right_x <- center_x + alp_bottom_half_width

        alp_poly <- data.frame(
          node = rep(i, 3),
          band = rep("alp", 3),
          x = c(top_x, alp_right_x, alp_left_x),
          y = c(alp_top_y, alp_bottom_y, alp_bottom_y),
          fill_color = rep(alp_color, 3),
          stringsAsFactors = FALSE
        )

        # Treeline (middle third) - medium trapezoidal section
        tl_top_y <- alp_bottom_y
        tl_bottom_y <- tl_top_y - band_height
        # Top of treeline matches bottom of alpine
        tl_top_left_x <- alp_left_x
        tl_top_right_x <- alp_right_x
        # Bottom of treeline (1/3 up from base): width = triangle_width * (2/3)
        tl_bottom_half_width <- get_width_at_height(band_height) / 2
        tl_bottom_left_x <- center_x - tl_bottom_half_width
        tl_bottom_right_x <- center_x + tl_bottom_half_width

        tl_poly <- data.frame(
          node = rep(i, 4),
          band = rep("tl", 4),
          x = c(tl_top_left_x, tl_top_right_x, tl_bottom_right_x, tl_bottom_left_x),
          y = c(tl_top_y, tl_top_y, tl_bottom_y, tl_bottom_y),
          fill_color = rep(tl_color, 4),
          stringsAsFactors = FALSE
        )

        # Below Treeline (bottom third) - widest trapezoidal section at base
        btl_top_y <- tl_bottom_y
        btl_bottom_y <- bottom_left_y
        # Top of BTL matches bottom of treeline
        btl_top_left_x <- tl_bottom_left_x
        btl_top_right_x <- tl_bottom_right_x
        # Bottom of BTL (at base): full width
        btl_bottom_left_x <- bottom_left_x
        btl_bottom_right_x <- bottom_right_x

        # Verify all coordinates are valid numeric values
        btl_x_coords <- c(btl_top_left_x, btl_top_right_x, bottom_right_x, bottom_left_x)
        btl_y_coords <- c(btl_top_y, btl_top_y, btl_bottom_y, btl_bottom_y)

        if (length(btl_x_coords) == 4 && length(btl_y_coords) == 4 &&
            all(is.finite(btl_x_coords)) && all(is.finite(btl_y_coords))) {
          btl_poly <- data.frame(
            node = rep(i, 4),
            band = rep("btl", 4),
            x = btl_x_coords,
            y = btl_y_coords,
            fill_color = rep(btl_color, 4),
            stringsAsFactors = FALSE
          )

          all_triangle_data <- rbind(all_triangle_data, alp_poly, tl_poly, btl_poly)
        }
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

    # Add all triangles
    if (nrow(all_triangle_data) > 0) {
      p <- p + ggplot2::geom_polygon(data = all_triangle_data,
                                     ggplot2::aes(x = x, y = y,
                                                  group = interaction(node, band),
                                                  fill = fill_color),
                                     color = "gray20",
                                     linewidth = 0.3)

      p <- p + ggplot2::scale_fill_identity()
    }

    p <- p + ggplot2::labs(title = "SOM Codebook Danger Patterns (Triangle)")

    return(p)
  }

  # Create plots for all three elevation bands
  p_alp <- create_elevation_plot(dataDanger$alp.used, "Alpine")
  p_tl <- create_elevation_plot(dataDanger$tl.used, "Treeline")
  p_btl <- create_elevation_plot(dataDanger$btl.used, "Below Treeline")

  # Create triangle plot
  p_triangle <- create_triangle_plot()

  # Return list of four plots
  return(list(
    alp = p_alp,
    tl = p_tl,
    btl = p_btl,
    triangle = p_triangle
  ))
}
