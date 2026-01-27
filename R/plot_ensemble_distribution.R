#' Plot Ensemble Prediction Distribution
#'
#' Creates visualizations of ensemble prediction distributions across danger levels
#' for all three elevation bands. Optionally compares predictions with actual values.
#'
#' @param ensemble_results A results object from \code{\link{ensemble_predict_som}}
#'   containing \code{ensemble_predictions}.
#' @param actual_danger Optional. A data frame with actual danger levels (alp.used,
#'   tl.used, btl.used). If provided, creates comparison plots. Default is NULL.
#'
#' @return A list containing:
#'   \describe{
#'     \item{distribution_plot}{ggplot object showing prediction distributions}
#'     \item{comparison_plot}{ggplot object comparing actual vs predicted (if
#'       \code{actual_danger} provided)}
#'     \item{distribution_data}{Data frame with distribution statistics}
#'   }
#'
#' @details
#' \strong{Distribution Plot:}
#' Shows the percentage of predictions for each danger level (1-5) across
#' the three elevation bands (ALP, TL, BTL). Each bar displays:
#' \itemize{
#'   \item Percentage of total predictions
#'   \item Count of observations
#'   \item Color-coded by danger level (green to black)
#' }
#'
#' \strong{Comparison Plot (when actual_danger provided):}
#' Creates side-by-side bars comparing actual vs predicted distributions:
#' \itemize{
#'   \item Actual values shown with 50% transparency
#'   \item Predicted values shown at full opacity
#'   \item Allows visual assessment of distribution match
#' }
#'
#' \strong{Danger Level Colors:}
#' \itemize{
#'   \item Level 1 (Low): Green (#50B848)
#'   \item Level 2 (Moderate): Yellow (#FFF200)
#'   \item Level 3 (Considerable): Orange (#F7941E)
#'   \item Level 4 (High): Red (#ED1C24)
#'   \item Level 5 (Extreme): Black (#231F20)
#' }
#'
#' @examples
#' \dontrun{
#' # Create ensemble predictions
#' ensemble <- ensemble_predict_som(som_models, test_data)
#'
#' # Plot distribution only
#' plots <- plot_ensemble_distribution(ensemble)
#' print(plots$distribution_plot)
#'
#' # Plot with comparison to actual
#' plots <- plot_ensemble_distribution(ensemble, actual_danger = test_data$danger)
#' print(plots$distribution_plot)
#' print(plots$comparison_plot)
#'
#' # Save plots
#' ggsave("ensemble_dist.png", plots$distribution_plot, width = 12, height = 4)
#' ggsave("ensemble_comp.png", plots$comparison_plot, width = 12, height = 4)
#'
#' # View distribution data
#' print(plots$distribution_data)
#' }
#'
#' @seealso \code{\link{ensemble_predict_som}}, \code{\link{evaluate_ensemble}}
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text facet_wrap scale_fill_manual labs theme_minimal theme element_text element_blank ylim
#' @export
plot_ensemble_distribution <- function(ensemble_results, actual_danger = NULL) {
  
  predictions <- ensemble_results$ensemble_predictions
  
  # Calculate percentage for each danger level
  calc_percentages <- function(x) {
    counts <- table(factor(x, levels = 1:5))
    percentages <- (counts / sum(counts)) * 100
    data.frame(
      danger_level = 1:5,
      percentage = as.numeric(percentages),
      count = as.numeric(counts)
    )
  }
  
  # Get percentages for each elevation band
  alp_dist <- calc_percentages(predictions$predicted_alp)
  alp_dist$band <- "ALP (Alpine)"
  
  tl_dist <- calc_percentages(predictions$predicted_tl)
  tl_dist$band <- "TL (Treeline)"
  
  btl_dist <- calc_percentages(predictions$predicted_btl)
  btl_dist$band <- "BTL (Below Treeline)"
  
  # Combine all data
  all_dist <- rbind(alp_dist, tl_dist, btl_dist)
  all_dist$band <- factor(all_dist$band, levels = c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"))
  
  # Define danger colors
  danger_colors <- c("1" = "#50B848", "2" = "#FFF200", "3" = "#F7941E", 
                     "4" = "#ED1C24", "5" = "#231F20")
  
  # Create histogram plot
  p <- ggplot2::ggplot(all_dist, ggplot2::aes(x = factor(danger_level), y = percentage, fill = factor(danger_level))) +
    ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%\n(n=%d)", percentage, count)), 
              vjust = -0.3, size = 3.5, fontface = "bold") +
    ggplot2::facet_wrap(~ band, ncol = 3) +
    ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
    ggplot2::labs(title = "Ensemble Prediction Distribution by Elevation Band",
         x = "Danger Level",
         y = "Percentage of Predictions (%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0, max(all_dist$percentage) * 1.15)
  
  # If actual data provided, also create comparison plot
  if (!is.null(actual_danger)) {
    actual_alp <- calc_percentages(actual_danger$alp.used)
    actual_alp$band <- "ALP (Alpine)"
    actual_alp$type <- "Actual"
    
    actual_tl <- calc_percentages(actual_danger$tl.used)
    actual_tl$band <- "TL (Treeline)"
    actual_tl$type <- "Actual"
    
    actual_btl <- calc_percentages(actual_danger$btl.used)
    actual_btl$band <- "BTL (Below Treeline)"
    actual_btl$type <- "Actual"
    
    alp_dist$type <- "Predicted"
    tl_dist$type <- "Predicted"
    btl_dist$type <- "Predicted"
    
    comparison_data <- rbind(
      rbind(actual_alp, alp_dist),
      rbind(actual_tl, tl_dist),
      rbind(actual_btl, btl_dist)
    )
    
    comparison_data$band <- factor(comparison_data$band, 
                                   levels = c("ALP (Alpine)", "TL (Treeline)", "BTL (Below Treeline)"))
    
    p_comparison <- ggplot2::ggplot(comparison_data, 
                           ggplot2::aes(x = factor(danger_level), y = percentage, fill = factor(danger_level), alpha = type)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.5) +
      ggplot2::facet_wrap(~ band, ncol = 3) +
      ggplot2::scale_fill_manual(values = danger_colors, name = "Danger Level") +
      ggplot2::scale_alpha_manual(values = c("Actual" = 0.5, "Predicted" = 1.0), name = "Type") +
      ggplot2::labs(title = "Actual vs. Predicted Distribution by Elevation Band",
           x = "Danger Level",
           y = "Percentage (%)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        strip.text = ggplot2::element_text(size = 12, face = "bold"),
        legend.position = "bottom"
      )
    
    return(list(
      distribution_plot = p,
      comparison_plot = p_comparison,
      distribution_data = all_dist
    ))
  }
  
  return(list(
    distribution_plot = p,
    distribution_data = all_dist
  ))
}
