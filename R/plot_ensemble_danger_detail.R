#' Plot Ensemble Danger Prediction with Triangle and Pooled Histograms
#'
#' Creates a visualization showing the ensemble's predicted danger levels as a
#' triangle and histograms of observed danger ratings pooled from all unique
#' days across the winning nodes from all models in the ensemble.
#'
#' @param som_models A list of trained supersom model objects.
#' @param test_data A list of matrices for prediction (same format as training data).
#' @param observation_index Integer. Which observation/day to visualize (row index in test_data).
#' @param is_normalized Logical. If TRUE, assumes danger ratings in models are
#'   normalized to 0-1 range. Default is FALSE.
#'
#' @return A ggplot object showing:
#'   \itemize{
#'     \item Center triangle colored by ensemble winning danger ratings
#'     \item Left histograms showing pooled observed dangers from winning nodes
#'     \item Sample size (total unique days)
#'   }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Maps the observation to a node in each SOM model
#'   \item Determines the "winning" danger rating for each elevation (majority vote)
#'   \item Colors the triangle according to these winning ratings
#'   \item Pools all unique training days from all winning nodes across all models
#'   \item Creates histograms showing the distribution of observed dangers in those pooled days
#' }
#'
#' @examples
#' \dontrun{
#' som_models <- list(model1$som, model2$som, model3$som)
#' test_data <- model1$split_info$test
#'
#' # Visualize prediction for day 10
#' plot_ensemble_danger_detail(som_models, test_data, observation_index = 10)
#' }
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_rect geom_text scale_fill_identity coord_fixed theme_void theme labs
#' @export
plot_ensemble_danger_detail <- function(som_models, test_data, observation_index = 1,
                                        is_normalized = FALSE) {

  # Define danger level colors
  danger_colors <- c("#50B848", "#FFF200", "#F7941E", "#ED1C24", "#000000")

  # Helper function to denormalize danger values
  denormalize_danger <- function(x) {
    if (is.na(x)) return(NA)
    levels <- c(0.2, 0.4, 0.6, 0.8, 1.0)
    target <- c(1, 2, 3, 4, 5)
    idx <- which.min(abs(x - levels))
    return(target[idx])
  }

  # Helper function to get color
  get_danger_color <- function(val) {
    if (is.na(val) || val < 0.5 || val > 5) {
      return("gray47")
    } else {
      return(danger_colors[round(val)])
    }
  }

  n_models <- length(som_models)

  # Remove 'danger' from test_data if present to avoid duplication
  test_data_clean <- test_data
  if ("danger" %in% names(test_data_clean)) {
    test_data_clean$danger <- NULL
  }

  # Storage for votes and winning nodes
  alp_votes <- integer(5)
  tl_votes <- integer(5)
  btl_votes <- integer(5)

  winning_nodes <- list()

  # Get prediction from each model
  for (m in 1:n_models) {
    som_model <- som_models[[m]]

    # Create newdata list for this observation
    newdata_list <- lapply(test_data_clean, function(mat) {
      if (is.matrix(mat)) {
        matrix(mat[observation_index, , drop = FALSE], nrow = 1)
      } else {
        mat[observation_index]
      }
    })

    # Predict which node this observation maps to
    pred <- predict(som_model, newdata = newdata_list)
    winning_node <- pred$unit.classif[1]

    # Get danger data from training
    danger_matrix <- som_model$data$danger

    # Denormalize if needed
    if (is_normalized) {
      danger_matrix <- apply(danger_matrix, 2, function(col) {
        sapply(col, denormalize_danger)
      })
    }

    # Get all observations assigned to this node during training
    node_assignments <- som_model$unit.classif
    node_indices <- which(node_assignments == winning_node)

    # Store winning node info
    winning_nodes[[m]] <- list(
      node = winning_node,
      indices = node_indices
    )

    # Get danger values for this node
    node_alp <- danger_matrix[node_indices, "alp.used"]
    node_tl <- danger_matrix[node_indices, "tl.used"]
    node_btl <- danger_matrix[node_indices, "btl.used"]

    # Filter valid values
    valid_alp <- node_alp[!is.na(node_alp) & node_alp >= 1 & node_alp <= 5]
    valid_tl <- node_tl[!is.na(node_tl) & node_tl >= 1 & node_tl <= 5]
    valid_btl <- node_btl[!is.na(node_btl) & node_btl >= 1 & node_btl <= 5]

    # Get mode (most common value) for this model's vote
    get_mode <- function(x) {
      if (length(x) == 0) return(NA)
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    alp_mode <- get_mode(valid_alp)
    tl_mode <- get_mode(valid_tl)
    btl_mode <- get_mode(valid_btl)

    # Cast votes
    if (!is.na(alp_mode)) alp_votes[alp_mode] <- alp_votes[alp_mode] + 1
    if (!is.na(tl_mode)) tl_votes[tl_mode] <- tl_votes[tl_mode] + 1
    if (!is.na(btl_mode)) btl_votes[btl_mode] <- btl_votes[btl_mode] + 1
  }

  # Determine ensemble winners (highest vote, ties go to higher danger)
  get_winner <- function(votes) {
    if (all(votes == 0)) return(NA)
    max_votes <- max(votes)
    tied <- which(votes == max_votes)
    return(max(tied))  # Conservative: choose higher danger in ties
  }

  ensemble_alp <- get_winner(alp_votes)
  ensemble_tl <- get_winner(tl_votes)
  ensemble_btl <- get_winner(btl_votes)

  # Pool all unique training day indices across all winning nodes
  all_indices <- unique(unlist(lapply(winning_nodes, function(x) x$indices)))

  # Collect danger observations for those unique pooled days.
  # NOTE: this assumes every model in the ensemble was trained on the same
  # underlying day-indexed dataset (som_models[[i]]$data$danger rows line up
  # across models) -- the same assumption n_total below already relies on.
  # If that ever stops being true (e.g. each model trained on a different
  # resample), this will need to look up each index against the specific
  # model(s) that produced it instead of a single reference matrix.
  reference_danger <- som_models[[1]]$data$danger

  if (is_normalized) {
    reference_danger <- apply(reference_danger, 2, function(col) {
      sapply(col, denormalize_danger)
    })
  }

  pooled_alp <- reference_danger[all_indices, "alp.used"]
  pooled_tl  <- reference_danger[all_indices, "tl.used"]
  pooled_btl <- reference_danger[all_indices, "btl.used"]

  # Filter valid values
  valid_pooled_alp <- pooled_alp[!is.na(pooled_alp) & pooled_alp >= 1 & pooled_alp <= 5]
  valid_pooled_tl <- pooled_tl[!is.na(pooled_tl) & pooled_tl >= 1 & pooled_tl <= 5]
  valid_pooled_btl <- pooled_btl[!is.na(pooled_btl) & pooled_btl >= 1 & pooled_btl <= 5]

  # Total sample size
  n_total <- length(all_indices)

  # Get colors for triangle based on ensemble winners
  alp_color <- get_danger_color(ensemble_alp)
  tl_color <- get_danger_color(ensemble_tl)
  btl_color <- get_danger_color(ensemble_btl)

  # Create main triangle
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

  # Create histograms for pooled observations
  hist_x_offset <- -1.5
  hist_width <- 0.15
  hist_max_height <- 0.35

  all_hist_data <- data.frame()

  # Function to create histogram bars
  create_hist_bars <- function(values, y_baseline, elevation_name) {
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

        bar_df <- data.frame(
          elevation = elevation_name,
          xmin = hist_x_offset + (i-1) * hist_width,
          xmax = hist_x_offset + i * hist_width,
          ymin = y_baseline,
          ymax = y_baseline + bar_height,
          fill_color = bar_color,
          stringsAsFactors = FALSE
        )
        hist_bars <- rbind(hist_bars, bar_df)
      }
    }
    return(hist_bars)
  }

  # Create histogram bars for each elevation using pooled data
  alp_hist <- create_hist_bars(valid_pooled_alp, alp_bottom_y, "Alpine")
  tl_hist <- create_hist_bars(valid_pooled_tl, tl_bottom_y, "Treeline")
  btl_hist <- create_hist_bars(valid_pooled_btl, btl_bottom_y, "Below Treeline")

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
  p <- p + ggplot2::labs(title = "Ensemble Predicted Danger Rating Distribution")

  return(p)
}
