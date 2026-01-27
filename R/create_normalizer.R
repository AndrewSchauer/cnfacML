#' Create a Min-Max Normalizer
#'
#' Creates a normalizer object that scales data to the range [0, 1] based on
#' the minimum and maximum values observed in the training data.
#'
#' @param data A data frame or matrix containing the training data used to
#'   fit normalization parameters.
#' @param na.rm Logical. Should missing values be removed when computing
#'   min and max? Default is TRUE.
#'
#' @return A list with class "normalizer" containing:
#'   \item{params}{List of normalization parameters (min, max, range) for each feature}
#'   \item{transform}{Function to normalize new data}
#'   \item{inverse_transform}{Function to denormalize data back to original scale}
#'
#' @details
#' The normalizer fits parameters on training data and applies them consistently
#' to new data, ensuring proper train/test separation and preventing data leakage.
#'
#' Min-max normalization formula: (x - min) / (max - min)
#' Inverse formula: x_normalized * (max - min) + min
#'
#' @examples
#' # Create training data
#' train_data <- data.frame(
#'   x = c(10, 20, 30, 40, 50),
#'   y = c(100, 200, 300, 400, 500)
#' )
#'
#' # Fit normalizer on training data
#' normalizer <- create_normalizer(train_data)
#'
#' # Transform training data
#' train_norm <- normalizer$transform(train_data)
#'
#' # Transform new data using same parameters
#' new_data <- data.frame(x = c(25, 35), y = c(250, 350))
#' new_norm <- normalizer$transform(new_data)
#'
#' # Transform back to original scale
#' original <- normalizer$inverse_transform(train_norm)
#'
#' @export
create_normalizer <- function(data, na.rm = TRUE) {
  # Input validation
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }

  if (nrow(data) == 0) {
    stop("'data' cannot be empty")
  }

  # Convert matrix to data frame for consistent handling
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  # Fit normalization parameters for each column
  params <- lapply(data, function(x) {
    if (!is.numeric(x)) {
      stop("All columns must be numeric")
    }

    min_val <- min(x, na.rm = na.rm)
    max_val <- max(x, na.rm = na.rm)
    range_val <- max_val - min_val

    # Handle constant columns (range = 0)
    if (range_val == 0) {
      warning("Column has zero variance (constant values). ",
              "Normalized values will be 0.")
      range_val <- 1  # Avoid division by zero
    }

    list(
      min = min_val,
      max = max_val,
      range = range_val
    )
  })

  # Create normalizer object
  normalizer <- list(
    params = params,

    transform = function(new_data) {
      # Input validation
      if (!is.data.frame(new_data) && !is.matrix(new_data)) {
        stop("'new_data' must be a data frame or matrix")
      }

      if (is.matrix(new_data)) {
        new_data <- as.data.frame(new_data)
      }

      # Check column names match
      if (!all(names(new_data) %in% names(params))) {
        missing_cols <- setdiff(names(new_data), names(params))
        stop("Column(s) not found in fitted parameters: ",
             paste(missing_cols, collapse = ", "))
      }

      # Apply normalization
      normalized <- mapply(
        function(x, p) (x - p$min) / p$range,
        new_data[names(params)],
        params,
        SIMPLIFY = FALSE
      )

      as.data.frame(normalized)
    },

    inverse_transform = function(normalized_data) {
      # Input validation
      if (!is.data.frame(normalized_data) && !is.matrix(normalized_data)) {
        stop("'normalized_data' must be a data frame or matrix")
      }

      if (is.matrix(normalized_data)) {
        normalized_data <- as.data.frame(normalized_data)
      }

      # Check column names match
      if (!all(names(normalized_data) %in% names(params))) {
        missing_cols <- setdiff(names(normalized_data), names(params))
        stop("Column(s) not found in fitted parameters: ",
             paste(missing_cols, collapse = ", "))
      }

      # Apply inverse transformation
      original <- mapply(
        function(x, p) x * p$range + p$min,
        normalized_data[names(params)],
        params,
        SIMPLIFY = FALSE
      )

      as.data.frame(original)
    }
  )

  # Set class for potential S3 methods
  class(normalizer) <- c("normalizer", "list")

  return(normalizer)
}

#' Print method for normalizer objects
#'
#' @param x A normalizer object
#' @param ... Additional arguments (not used)
#'
#' @export
print.normalizer <- function(x, ...) {
  cat("Min-Max Normalizer\n")
  cat("------------------\n")
  cat("Number of features:", length(x$params), "\n")
  cat("Feature names:", paste(names(x$params), collapse = ", "), "\n\n")

  cat("Normalization parameters:\n")
  param_df <- data.frame(
    feature = names(x$params),
    min = sapply(x$params, function(p) p$min),
    max = sapply(x$params, function(p) p$max),
    range = sapply(x$params, function(p) p$range)
  )
  print(param_df, row.names = FALSE)

  invisible(x)
}
