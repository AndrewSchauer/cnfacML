#' Denormalize Danger Data
#'
#' Converts danger ratings from 0-1 normalized range back to standard 1-5 scale.
#' This is a standalone helper function for use with normalized avalanche danger data
#' from SOM models.
#'
#' @param danger_data A data frame or matrix with danger columns (e.g., alp.used, 
#'   tl.used, btl.used) in normalized 0-1 range. Can also be a numeric vector for
#'   a single elevation band.
#' @param verbose Logical. If TRUE, prints conversion information and diagnostics. 
#'   Default is TRUE.
#'
#' @return A data frame (or vector if input was a vector) with denormalized danger 
#'   values in 1-5 scale. Column names are preserved from input.
#'
#' @details
#' \strong{Conversion Scheme:}
#' 
#' The function converts normalized danger values using the standard scheme:
#' \itemize{
#'   \item 0.0 or NA -> NA (invalid/missing)
#'   \item 0.2 -> 1 (Low)
#'   \item 0.4 -> 2 (Moderate)
#'   \item 0.6 -> 3 (Considerable)
#'   \item 0.8 -> 4 (High)
#'   \item 1.0 -> 5 (Extreme)
#' }
#' 
#' \strong{Formula:}
#' 
#' The denormalization uses: `danger = round((normalized - 0.2) / 0.2) + 1`
#' 
#' Values are clamped to [1, 5] range to handle any floating-point precision issues.
#' 
#' \strong{Input Handling:}
#' \itemize{
#'   \item Accepts data frames, matrices, or numeric vectors
#'   \item For data frames/matrices without proper column names, assigns default names
#'   \item Preserves NA values and converts 0 values to NA
#'   \item Returns same data type as input (data frame or vector)
#' }
#' 
#' \strong{Diagnostic Output:}
#' 
#' When verbose = TRUE, the function prints:
#' \itemize{
#'   \item Original value ranges (0-1 scale)
#'   \item Denormalized value ranges (1-5 scale)
#'   \item Distribution table of denormalized values
#'   \item Number of NA/missing values
#' }
#'
#' @note
#' This function uses the same denormalization logic as \code{ensemble_predict_som}
#' and \code{ensemble_predict_danger} when \code{is_normalized = TRUE}. Use this
#' function to denormalize actual danger data before passing it to evaluation
#' functions like \code{evaluate_ensemble}.
#'
#' @examples
#' \dontrun{
#' # ========================================================================
#' # EXAMPLE 1: Denormalize test danger data for evaluation
#' # ========================================================================
#' 
#' # Get ensemble predictions with normalized data
#' ensemble <- ensemble_predict_som(
#'   som_models = model_list,
#'   test_data = best_som$split_info$test,
#'   predict_data = best_som$split_info$test$danger,
#'   is_normalized = TRUE  # This denormalizes predictions
#' )
#' 
#' # Denormalize actual danger data to match
#' actual_danger <- denormalize_danger_data(
#'   best_som$split_info$test$danger
#' )
#' 
#' # Now both are in 1-5 scale - evaluate!
#' eval_results <- evaluate_ensemble(
#'   ensemble_results = ensemble,
#'   actual_danger = actual_danger
#' )
#' 
#' # ========================================================================
#' # EXAMPLE 2: Denormalize a single elevation band
#' # ========================================================================
#' 
#' # Extract just alpine danger (normalized)
#' alp_normalized <- best_som$split_info$test$danger$alp.used
#' 
#' # Denormalize it
#' alp_denormalized <- denormalize_danger_data(alp_normalized)
#' 
#' # ========================================================================
#' # EXAMPLE 3: Silent mode (no diagnostic output)
#' # ========================================================================
#' 
#' actual_danger <- denormalize_danger_data(
#'   best_som$split_info$test$danger,
#'   verbose = FALSE  # No output
#' )
#' 
#' # ========================================================================
#' # EXAMPLE 4: Verify the conversion
#' # ========================================================================
#' 
#' # Original normalized values
#' normalized_data <- data.frame(
#'   alp.used = c(0.2, 0.4, 0.6, 0.8, 1.0, NA),
#'   tl.used = c(0.2, 0.2, 0.4, 0.6, 0.8, 0.0),
#'   btl.used = c(0.2, 0.4, 0.4, 0.6, 0.8, 1.0)
#' )
#' 
#' # Denormalize
#' denormalized <- denormalize_danger_data(normalized_data)
#' 
#' # Expected output:
#' # alp.used: 1, 2, 3, 4, 5, NA
#' # tl.used: 1, 1, 2, 3, 4, NA
#' # btl.used: 1, 2, 2, 3, 4, 5
#' 
#' # ========================================================================
#' # EXAMPLE 5: Use in a workflow
#' # ========================================================================
#' 
#' # Load your data
#' source("your_som_functions.R")
#' 
#' # Train models with normalized data
#' best_som <- find_best_som_seasonal(
#'   data_list = normalized_data_list,
#'   date_column = "date",
#'   seed = 42
#' )
#' 
#' # Create ensemble
#' ensemble <- ensemble_predict_som(
#'   som_models = model_list,
#'   test_data = best_som$split_info$test,
#'   predict_data = best_som$split_info$test$danger,
#'   is_normalized = TRUE
#' )
#' 
#' # Denormalize actual values
#' actual_danger <- denormalize_danger_data(
#'   best_som$split_info$test$danger
#' )
#' 
#' # Evaluate
#' eval_results <- evaluate_ensemble(ensemble, actual_danger)
#' 
#' # Compare predictions
#' comparison <- data.frame(
#'   actual_alp = actual_danger$alp.used,
#'   predicted_alp = ensemble$ensemble_predictions$predicted_alp,
#'   correct = actual_danger$alp.used == ensemble$ensemble_predictions$predicted_alp
#' )
#' 
#' accuracy <- mean(comparison$correct, na.rm = TRUE)
#' cat("Accuracy:", round(accuracy * 100, 2), "%\n")
#' }
#'
#' @seealso
#' \code{\link{ensemble_predict_som}}, \code{\link{ensemble_predict_danger}},
#' \code{\link{evaluate_ensemble}}
#'
#' @export
denormalize_danger_data <- function(danger_data, verbose = TRUE) {
  
  # Check if input is a vector (single elevation band)
  is_vector_input <- is.vector(danger_data) || (is.matrix(danger_data) && ncol(danger_data) == 1)
  
  # Helper function to denormalize a single value
  denormalize_danger <- function(norm_value) {
    if (is.na(norm_value) || norm_value == 0) {
      return(NA)
    }
    # Convert from 0-1 normalized range back to 1-5 scale
    # Normalization scheme: 0.2=1, 0.4=2, 0.6=3, 0.8=4, 1.0=5
    danger_val <- round((norm_value - 0.2) / 0.2) + 1
    danger_val <- pmax(1, pmin(5, danger_val))  # Ensure it's between 1 and 5
    return(danger_val)
  }
  
  # Vectorized version
  denormalize_danger_vec <- function(norm_vec) {
    sapply(norm_vec, denormalize_danger)
  }
  
  if (verbose) {
    cat("\n")
    cat("=======================================================================\n")
    cat("DENORMALIZING DANGER DATA\n")
    cat("=======================================================================\n")
    cat("\nConverting from 0-1 normalized range to 1-5 scale\n")
    cat("Normalization scheme: 0.2=1, 0.4=2, 0.6=3, 0.8=4, 1.0=5\n")
    cat("\nNA and 0 values preserved as NA\n")
  }
  
  # Handle vector input
  if (is_vector_input) {
    if (verbose) {
      cat("\nInput type: Vector (single elevation band)\n")
      cat("\nOriginal range: ", round(min(danger_data, na.rm = TRUE), 3), " to ", 
          round(max(danger_data, na.rm = TRUE), 3), "\n", sep = "")
    }
    
    result <- denormalize_danger_vec(danger_data)
    
    if (verbose) {
      cat("Denormalized range: ", min(result, na.rm = TRUE), " to ", 
          max(result, na.rm = TRUE), "\n", sep = "")
      cat("\nValue distribution:\n")
      print(table(result, useNA = "ifany"))
      cat("\n")
    }
    
    return(result)
  }
  
  # Handle data frame/matrix input
  if (verbose) {
    cat("\nInput type: Data frame/matrix (", nrow(danger_data), " rows, ", 
        ncol(danger_data), " columns)\n", sep = "")
  }
  
  # Convert to data frame if needed
  danger_df <- as.data.frame(danger_data)
  
  # Ensure column names
  if (!all(c("alp.used", "tl.used", "btl.used") %in% colnames(danger_df))) {
    if (ncol(danger_df) == 3) {
      if (verbose) {
        cat("\nNote: Assigning default column names (alp.used, tl.used, btl.used)\n")
      }
      colnames(danger_df) <- c("alp.used", "tl.used", "btl.used")
    } else {
      stop("danger_data must have 3 columns (alp.used, tl.used, btl.used) or be a single vector")
    }
  }
  
  # Store original values for reporting
  if (verbose) {
    cat("\nOriginal ranges (0-1 scale):\n")
    cat("  ALP.USED : ", sprintf("%.3f", min(danger_df$alp.used, na.rm = TRUE)), " to ", 
        sprintf("%.3f", max(danger_df$alp.used, na.rm = TRUE)), 
        " (", sum(is.na(danger_df$alp.used)), " NAs)\n", sep = "")
    cat("  TL.USED  : ", sprintf("%.3f", min(danger_df$tl.used, na.rm = TRUE)), " to ", 
        sprintf("%.3f", max(danger_df$tl.used, na.rm = TRUE)),
        " (", sum(is.na(danger_df$tl.used)), " NAs)\n", sep = "")
    cat("  BTL.USED : ", sprintf("%.3f", min(danger_df$btl.used, na.rm = TRUE)), " to ", 
        sprintf("%.3f", max(danger_df$btl.used, na.rm = TRUE)),
        " (", sum(is.na(danger_df$btl.used)), " NAs)\n", sep = "")
  }
  
  # Denormalize each column
  danger_df$alp.used <- denormalize_danger_vec(danger_df$alp.used)
  danger_df$tl.used <- denormalize_danger_vec(danger_df$tl.used)
  danger_df$btl.used <- denormalize_danger_vec(danger_df$btl.used)
  
  if (verbose) {
    cat("\nDenormalized ranges (1-5 scale):\n")
    cat("  ALP.USED : ", min(danger_df$alp.used, na.rm = TRUE), " to ", 
        max(danger_df$alp.used, na.rm = TRUE),
        " (", sum(is.na(danger_df$alp.used)), " NAs)\n", sep = "")
    cat("  TL.USED  : ", min(danger_df$tl.used, na.rm = TRUE), " to ", 
        max(danger_df$tl.used, na.rm = TRUE),
        " (", sum(is.na(danger_df$tl.used)), " NAs)\n", sep = "")
    cat("  BTL.USED : ", min(danger_df$btl.used, na.rm = TRUE), " to ", 
        max(danger_df$btl.used, na.rm = TRUE),
        " (", sum(is.na(danger_df$btl.used)), " NAs)\n", sep = "")
    
    cat("\nValue distributions:\n")
    cat("\nALP.USED:\n")
    print(table(danger_df$alp.used, useNA = "ifany"))
    cat("\nTL.USED:\n")
    print(table(danger_df$tl.used, useNA = "ifany"))
    cat("\nBTL.USED:\n")
    print(table(danger_df$btl.used, useNA = "ifany"))
    cat("\n")
    cat("=======================================================================\n")
    cat("DENORMALIZATION COMPLETE\n")
    cat("=======================================================================\n\n")
  }
  
  return(danger_df)
}
