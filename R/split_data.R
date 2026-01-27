#' Split Data into Training and Test Sets
#'
#' Splits avalanche forecasting data into training and test sets using either
#' random sampling or sequential/temporal splitting.
#'
#' @param dataNumeric A data frame or matrix containing numeric predictor variables.
#' @param dataBinary A data frame or matrix containing binary predictor variables.
#' @param dataDanger A data frame or matrix containing danger level outcomes.
#' @param test_size Numeric between 0 and 1. Proportion of data to use for testing.
#'   Default is 0.2 (20% test, 80% train).
#' @param seed Integer. Random seed for reproducible splits when using random method.
#'   Default is 123.
#' @param split_method Character. Method for splitting data. Options:
#'   \itemize{
#'     \item \code{"random"}: Random sampling (default)
#'     \item \code{"sequential"} or \code{"temporal"}: Last observations as test set
#'   }
#'
#' @return A list containing:
#'   \describe{
#'     \item{train}{List with three elements (numeric, binary, danger) containing
#'       the training data subsets}
#'     \item{test}{List with three elements (numeric, binary, danger) containing
#'       the test data subsets}
#'     \item{train_indices}{Integer vector of row indices used for training}
#'     \item{test_indices}{Integer vector of row indices used for testing}
#'     \item{split_method}{Character string indicating which split method was used}
#'   }
#'
#' @details
#' This function supports two splitting strategies:
#'
#' \strong{Random Split:}
#' \itemize{
#'   \item Randomly samples observations for test set
#'   \item Good for general model evaluation
#'   \item Assumes observations are independent
#'   \item Uses \code{seed} for reproducibility
#' }
#'
#' \strong{Sequential/Temporal Split:}
#' \itemize{
#'   \item Uses last \code{test_size} proportion of observations as test set
#'   \item Preserves temporal ordering
#'   \item Simulates forecasting on future data
#'   \item No randomization involved
#' }
#'
#' The function prints the split method used to the console.
#'
#' @note
#' For time-series or temporal data, consider using the sequential split or
#' \code{\link{split_data_by_season}} for more robust temporal validation.
#'
#' @examples
#' \dontrun{
#' # Random split (default)
#' split <- split_data(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,
#'   test_size = 0.2,
#'   seed = 42
#' )
#'
#' # Sequential/temporal split
#' split <- split_data(
#'   dataNumeric = my_numeric_data,
#'   dataBinary = my_binary_data,
#'   dataDanger = my_danger_data,
#'   test_size = 0.2,
#'   split_method = "sequential"
#' )
#'
#' # Access training and test data
#' train_numeric <- split$train$numeric
#' test_danger <- split$test$danger
#' }
#'
#' @seealso \code{\link{split_data_by_season}} for season-based splitting
#'
#' @export
split_data <- function(dataNumeric, dataBinary, dataDanger, test_size = 0.2, seed = 123, split_method = "random") {
  
  n <- nrow(dataNumeric)
  n_test <- floor(n * test_size)
  
  if (split_method == "random") {
    # Random split
    set.seed(seed)
    test_indices <- sample(1:n, size = n_test)
    train_indices <- setdiff(1:n, test_indices)
    
  } else if (split_method == "sequential" || split_method == "temporal") {
    # Sequential/temporal split - last observations as test
    train_indices <- 1:(n - n_test)
    test_indices <- (n - n_test + 1):n
    
  } else {
    stop("Invalid split_method. Choose 'random' or 'sequential'.")
  }
  
  cat("   Split method:", split_method, "\n")
  
  return(list(
    train = list(
      numeric = dataNumeric[train_indices, ],
      binary = dataBinary[train_indices, ],
      danger = dataDanger[train_indices, ]
    ),
    test = list(
      numeric = dataNumeric[test_indices, ],
      binary = dataBinary[test_indices, ],
      danger = dataDanger[test_indices, ]
    ),
    train_indices = train_indices,
    test_indices = test_indices,
    split_method = split_method
  ))
}
