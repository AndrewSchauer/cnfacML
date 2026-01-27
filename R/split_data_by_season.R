#' Split Data by Avalanche Season
#'
#' Splits avalanche forecasting data into training and test sets based on complete
#' avalanche seasons rather than random sampling. This ensures temporal coherence
#' in the evaluation of predictive models.
#'
#' @param data_list A named list of data frames or matrices to split. All matrices
#'   must have the same number of rows. Example: 
#'   \code{list(numeric = dataNumeric, binary = dataBinary, danger = dataDanger)}
#' @param date_column A vector of dates corresponding to the rows in the data.
#'   Can be Date objects or character strings in MM/DD/YYYY format.
#' @param n_test_seasons Integer. Number of seasons to use for the test set.
#'   Only used if \code{test_seasons} is NULL. Default is 3.
#' @param test_seasons Integer vector. Specific season years to use for testing.
#'   If NULL, seasons are randomly selected. Default is NULL.
#' @param seed Integer. Random seed for reproducible season selection when
#'   \code{test_seasons} is NULL. Default is 123.
#' @param danger_col_name Character. Name of the danger data element in data_list
#'   for distribution diagnostics. Default is "danger".
#'
#' @return A list containing:
#'   \describe{
#'     \item{train}{Named list with same names as input data_list, containing
#'       the training data subsets}
#'     \item{test}{Named list with same names as input data_list, containing
#'       the test data subsets}
#'     \item{train_indices}{Integer vector of row indices used for training}
#'     \item{test_indices}{Integer vector of row indices used for testing}
#'     \item{train_seasons}{Integer vector of season years used for training}
#'     \item{test_seasons}{Integer vector of season years used for testing}
#'     \item{all_seasons}{Integer vector of all unique season years in the data}
#'     \item{split_method}{Character string: "season-based"}
#'   }
#'
#' @details
#' This function performs season-based data splitting for avalanche forecasting models.
#' It ensures that entire seasons are kept together in either the training or test set,
#' which is important for:
#' \itemize{
#'   \item Evaluating model performance on unseen temporal periods
#'   \item Avoiding data leakage from temporal autocorrelation
#'   \item Simulating real-world forecasting scenarios
#' }
#'
#' The function prints diagnostic information including:
#' \itemize{
#'   \item Total observations and date range
#'   \item Number of unique seasons found
#'   \item Selected test seasons and their sample sizes
#'   \item Training/test split proportions
#'   \item Danger level distributions in both sets (if danger data provided)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with 3 matrices
#' data_list <- list(
#'   numeric = dataNumeric,
#'   binary = dataBinary,
#'   danger = dataDanger
#' )
#' 
#' split <- split_data_by_season(
#'   data_list = data_list,
#'   date_column = my_dates,
#'   n_test_seasons = 3,
#'   seed = 123
#' )
#'
#' # With additional matrix (e.g., previous danger ratings)
#' data_list <- list(
#'   numeric = dataNumeric,
#'   binary = dataBinary,
#'   danger = dataDanger,
#'   dangerPrev = dataDangerPrev
#' )
#' 
#' split <- split_data_by_season(
#'   data_list = data_list,
#'   date_column = my_dates,
#'   test_seasons = c(2015, 2018, 2022)
#' )
#'
#' # Access training data
#' train_numeric <- split$train$numeric
#' train_danger <- split$train$danger
#' train_dangerprev <- split$train$dangerPrev
#' }
#'
#' @seealso \code{\link{identify_avalanche_seasons}}
#'
#' @export
split_data_by_season <- function(data_list, 
                                 date_column,
                                 n_test_seasons = 3,
                                 test_seasons = NULL,
                                 seed = 123,
                                 danger_col_name = "danger") {
  
  # Validate input
  if (!is.list(data_list)) {
    stop("data_list must be a list of data frames or matrices")
  }
  
  if (length(data_list) == 0) {
    stop("data_list is empty")
  }
  
  if (is.null(names(data_list))) {
    stop("data_list must have named elements")
  }
  
  # Check that all elements have the same number of rows
  n_rows <- sapply(data_list, nrow)
  if (length(unique(n_rows)) > 1) {
    stop("All elements in data_list must have the same number of rows. Found: ",
         paste(names(data_list), "=", n_rows, collapse = ", "))
  }
  
  n_obs <- n_rows[1]
  
  # Check date_column length
  if (length(date_column) != n_obs) {
    stop("date_column length (", length(date_column), 
         ") does not match data rows (", n_obs, ")")
  }
  
  # Identify seasons for each observation
  seasons <- identify_avalanche_seasons(date_column)
  
  # Get unique seasons
  unique_seasons <- sort(unique(seasons))
  n_seasons <- length(unique_seasons)
  
  cat("\n=== Season-Based Data Split ===\n")
  cat("Total observations:", n_obs, "\n")
  cat("Number of data matrices:", length(data_list), 
      "(", paste(names(data_list), collapse = ", "), ")\n")
  cat("Date range:", min(date_column), "to", max(date_column), "\n")
  cat("Unique seasons found:", n_seasons, "\n")
  cat("Seasons:", paste(paste0(unique_seasons, "/", unique_seasons + 1), collapse = ", "), "\n\n")
  
  # Select test seasons
  if (is.null(test_seasons)) {
    # Randomly select n_test_seasons
    set.seed(seed)
    test_seasons <- sample(unique_seasons, size = min(n_test_seasons, n_seasons))
    cat("Randomly selected test seasons (seed =", seed, "):\n")
  } else {
    cat("Using specified test seasons:\n")
  }
  
  # Print test seasons
  for (s in sort(test_seasons)) {
    season_indices <- which(seasons == s)
    cat("  ", s, "/", s + 1, " (n = ", length(season_indices), ")\n", sep = "")
  }
  
  # Create train/test indices
  test_indices <- which(seasons %in% test_seasons)
  train_indices <- which(!seasons %in% test_seasons)
  
  cat("\nTraining observations:", length(train_indices), 
      "(", round(length(train_indices) / n_obs * 100, 1), "%)\n")
  cat("Test observations:", length(test_indices), 
      "(", round(length(test_indices) / n_obs * 100, 1), "%)\n")
  
  # Check for distribution differences (if danger data available)
  if (danger_col_name %in% names(data_list)) {
    dataDanger <- data_list[[danger_col_name]]
    
    # Check if it has the expected columns
    if (all(c("alp.used", "tl.used", "btl.used") %in% colnames(dataDanger))) {
      cat("\n=== Danger Level Distribution ===\n")
      cat("Training ALP distribution:\n")
      print(round(prop.table(table(dataDanger[train_indices, "alp.used"])) * 100, 1))
      cat("\nTest ALP distribution:\n")
      print(round(prop.table(table(dataDanger[test_indices, "alp.used"])) * 100, 1))
    }
  }
  
  # Split all data matrices and convert to data frames for $ access
  train_list <- list()
  test_list <- list()
  
  for (name in names(data_list)) {
    train_subset <- data_list[[name]][train_indices, , drop = FALSE]
    test_subset <- data_list[[name]][test_indices, , drop = FALSE]
    
    # Convert to data frame to allow $ operator
    train_list[[name]] <- as.data.frame(train_subset)
    test_list[[name]] <- as.data.frame(test_subset)
  }
  
  return(list(
    train = train_list,
    test = test_list,
    train_indices = train_indices,
    test_indices = test_indices,
    train_seasons = unique_seasons[!unique_seasons %in% test_seasons],
    test_seasons = test_seasons,
    all_seasons = unique_seasons,
    split_method = "season-based"
  ))
}
