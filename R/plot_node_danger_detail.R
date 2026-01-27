#' Plot Detailed Danger Analysis for a Single SOM Node
#'
#' Creates a detailed visualization for a specific SOM node showing the codebook
#' danger triangle and histograms of observed danger ratings for each elevation band.
#'
#' @param SOMit A trained SOM model object (from kohonen package).
#' @param dataDanger A data frame with danger levels containing columns:
#'   \code{alp.used}, \code{tl.used}, \code{btl.used}.
#' @param node_id Integer. The node number to visualize (1 to number of nodes).
#'
#' @return A ggplot object showing the node's danger patterns.
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Central triangle showing codebook danger values (learned pattern)
#'   \item Three small histograms (left side) showing observed danger distributions
#'   \item Sample size displayed below the triangle
#' }
#'
#' @examples
#' \dontrun{
#' # Plot details for node 5
#' plot_node_danger_detail(som_model, dataDanger, node_id = 5)
#'
#' # Save the plot
#' p <- plot_node_danger_detail(som_model, dataDanger, node_id = 5)
#' ggsave("node_5_detail.png", p, width = 8, height = 8, dpi = 300)
#' }
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_rect geom_text scale_fill_identity coord_fixed theme_void theme labs
#' @export
plot_node_danger_detail <- function(SOMit, dataDanger, node_id) {

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

  # Get observations assigned to this node
  node_obs_alp <- dataDanger$alp.used[node_assignments == node_id]
  node_obs_tl <- dataDanger$tl.used[node_assignments == node_id]
  node_obs_btl <- dataDanger$btl.used[node_assignments == node_id]

  # Filter valid observations (1-5 range)
  valid_alp <- node_obs_alp[!is.na(node_obs_alp) & node_obs_alp >= 1 & node_obs_alp <= 5]
  valid_tl <- node_obs_tl[!is.na(node_obs_tl) & node_obs_tl >= 1 & node_obs_tl <= 5]
  valid_btl <- node_obs_btl[!is.na(node_obs_btl) & node_obs_btl >= 1 & node_obs_btl <= 5]

  # Get total sample size
  n_total <- length(node_obs_alp)

  # Get codebook values
  codebook_danger <- SOMit$codes$danger
  alp_code <- codebook_danger[node_id, "alp.used"]
  tl_code <- codebook_danger[node_id, "tl.used"]
  btl_code <- codebook_danger[node_id, "btl.used"]

  # Helper function to get color
  get_danger_color <- function(val) {
    if (is.na(val) || val < 0.5 || val > 5) {
      return("gray47")
    } else {
      return(danger_colors[round(val)])
    }
  }

  # Get colors for codebook triangle
  alp_color <- get_danger_color(alp_code)
  tl_color <- get_danger_color(tl_code)
  btl_color <- get_danger_color(btl_code)

  # Create main triangle (centered at origin, we'll adjust coordinates later)
  triangle_width <- 1.5
  triangle_height <- 1.3
  center_x <- 0
  center_y <- 0

  # Calculate triangle vertices
  top_x <- center_x
  top_y <- center_y + triangle_height/2
  bottom_left_x <- center_x - triangle_width/2
  bottom_left_y <- center_y - triangle_height/2
  bottom_right_x <- center_x + triangle_width/2
  bottom_right_y <- center_y - triangle_height/2

  # Helper function to get width at height
  band_height <- triangle_height / 3
  get_width_at_height <- function(height_from_bottom) {
    return(triangle_width * (1 - height_from_bottom / triangle_height))
  }

  # Alpine (top third)
  alp_top_y <- top_y
  alp_bottom_y <- top_y - band_height
  alp_bottom_half_width <- get_width_at_height(2 * band_height) / 2
  alp_left_x <- center_x - alp_bottom_half_width
  alp_right_x <- center_x + alp_bottom_half_width

  alp_poly <- data.frame(
    band = "alp",
    x = c(top_x, alp_right_x, alp_left_x),
    y = c(alp_top_y, alp_bottom_y, alp_bottom_y),
    fill_color = alp_color,
    stringsAsFactors = FALSE
  )

  # Treeline (middle third)
  tl_top_y <- alp_bottom_y
  tl_bottom_y <- tl_top_y - band_height
  tl_top_left_x <- alp_left_x
  tl_top_right_x <- alp_right_x
  tl_bottom_half_width <- get_width_at_height(band_height) / 2
  tl_bottom_left_x <- center_x - tl_bottom_half_width
  tl_bottom_right_x <- center_x + tl_bottom_half_width

  tl_poly <- data.frame(
    band = "tl",
    x = c(tl_top_left_x, tl_top_right_x, tl_bottom_right_x, tl_bottom_left_x),
    y = c(tl_top_y, tl_top_y, tl_bottom_y, tl_bottom_y),
    fill_color = tl_color,
    stringsAsFactors = FALSE
  )

  # Below Treeline (bottom third)
  btl_top_y <- tl_bottom_y
  btl_bottom_y <- bottom_left_y
  btl_top_left_x <- tl_bottom_left_x
  btl_top_right_x <- tl_bottom_right_x
  btl_bottom_left_x <- bottom_left_x
  btl_bottom_right_x <- bottom_right_x

  btl_poly <- data.frame(
    band = "btl",
    x = c(btl_top_left_x, btl_top_right_x, btl_bottom_right_x, btl_bottom_left_x),
    y = c(btl_top_y, btl_top_y, btl_bottom_y, btl_bottom_y),
    fill_color = btl_color,
    stringsAsFactors = FALSE
  )

  triangle_data <- rbind(alp_poly, tl_poly, btl_poly)

  # Create histograms for each elevation band
  # Position histograms to the left of the triangle
  hist_x_offset <- -1.5  # Left of triangle
  hist_width <- 0.15
  hist_max_height <- 0.35

  all_hist_data <- data.frame()

  # Function to create histogram bars
  create_hist_bars <- function(values, y_center, elevation_name) {
    if (length(values) == 0) return(NULL)

    h <- hist(values, breaks = seq(0.5, 5.5, by = 1), plot = FALSE)
    max_count <- max(h$counts)

    if (max_count == 0) return(NULL)

    hist_bars <- data.frame()
    for (i in 1:length(h$counts)) {
      if (h$counts[i] > 0) {
        bar_height <- (h$counts[i] / max_count) * hist_max_height
        danger_level <- round(h$mids[i])
        bar_color <- danger_colors[danger_level]

        # Align bars horizontally from the same baseline
        bar_df <- data.frame(
          elevation = elevation_name,
          xmin = hist_x_offset + (i-1) * hist_width,
          xmax = hist_x_offset + i * hist_width,
          ymin = y_center,  # All bars start from same baseline
          ymax = y_center + bar_height,  # Bars extend upward
          fill_color = bar_color,
          stringsAsFactors = FALSE
        )
        hist_bars <- rbind(hist_bars, bar_df)
      }
    }
    return(hist_bars)
  }

  # Create histogram bars for each elevation
  # Positions aligned with triangle bands - use bottom of each band as baseline
  alp_hist <- create_hist_bars(valid_alp, alp_bottom_y, "Alpine")
  tl_hist <- create_hist_bars(valid_tl, tl_bottom_y, "Treeline")
  btl_hist <- create_hist_bars(valid_btl, btl_bottom_y, "Below Treeline")

  if (!is.null(alp_hist)) all_hist_data <- rbind(all_hist_data, alp_hist)
  if (!is.null(tl_hist)) all_hist_data <- rbind(all_hist_data, tl_hist)
  if (!is.null(btl_hist)) all_hist_data <- rbind(all_hist_data, btl_hist)

  # Create the plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))

  # Add triangle
  p <- p + ggplot2::geom_polygon(data = triangle_data,
                                 ggplot2::aes(x = x, y = y, group = band, fill = fill_color),
                                 color = "gray20",
                                 linewidth = 0.8)

  # Add histograms
  if (nrow(all_hist_data) > 0) {
    p <- p + ggplot2::geom_rect(data = all_hist_data,
                                ggplot2::aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax,
                                             fill = fill_color),
                                color = "gray30",
                                linewidth = 0.3)
  }

  p <- p + ggplot2::scale_fill_identity()

  # Add sample size label
  p <- p + ggplot2::geom_text(ggplot2::aes(x = center_x, y = bottom_left_y - 0.3),
                              label = paste0("n = ", n_total),
                              size = 5,
                              hjust = 0.5)

  # Add title
  p <- p + ggplot2::labs(title = "Predicted Danger Rating Distribution")

  return(p)
}
