# Install and load required package
# install.packages("snotelr")
library(snotelr)

#' Fetch and Save SNOTEL Data
#'
#' Downloads SNOTEL data for a specified station and time period, then saves it as a CSV file.
#'
#' @param station_id Numeric. SNOTEL station ID number (e.g., 954 for Turnagain Pass)
#' @param station_name Character. Optional name to include in the output filename
#' @param start_date Character. Start date in "YYYY-MM-DD" format. If NULL, returns all available data
#' @param end_date Character. End date in "YYYY-MM-DD" format. If NULL, returns all available data
#' @param interval Character. Either "daily" or "hourly" for observation frequency. Default is "daily"
#' @param period Character. Either "end" or "start" to specify period values. Default is "end"
#'   - "end": Values represent end-of-day measurements (SNOTEL's native reporting)
#'   - "start": Dates shifted forward by 1 day to represent start-of-day values
#' @param output_file Character. Custom filename for CSV output. If NULL, auto-generates filename
#'
#' @return A data frame containing:
#'   - date: Date of observation
#'   - snow_depth_in: Snow depth in inches
#'   - swe_in: Snow water equivalent in inches
#'   - precip_in: Accumulated precipitation in inches
#'   - temp_max_f: Maximum temperature in Fahrenheit
#'   - temp_min_f: Minimum temperature in Fahrenheit
#'
#' @examples
#' # Get full record for Turnagain Pass
#' data <- get_snotel_data(station_id = 954)
#'
#' # Get winter season data with custom date range
#' data <- get_snotel_data(station_id = 954,
#'                         start_date = "2023-10-01",
#'                         end_date = "2024-05-31")
#'
# Function to fetch and save SNOTEL data
get_snotel_data <- function(station_id,
                            station_name = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            interval = "daily",
                            period = "end",
                            output_file = NULL) {

  # Validate interval argument
  if (!interval %in% c("daily", "hourly")) {
    stop("interval must be 'daily' or 'hourly'")
  }

  # Validate period argument
  if (!period %in% c("start", "end")) {
    stop("period must be 'start' or 'end'")
  }

  # Download SNOTEL data with specified interval
  if (interval == "hourly") {
    snotel_data <- snotel_download(site_id = station_id, internal = TRUE, path = tempdir())
    # Note: hourly data requires different handling - snotelr may not support this directly
    warning("Hourly data support may be limited. Check snotelr documentation.")
  } else {
    snotel_data <- snotel_download(site_id = station_id, internal = TRUE)
  }

  # Select and rename relevant columns
  # Note: snotelr returns metric units by default (mm for depth/SWE/precip, C for temp)
  # Convert to imperial units and round to nearest tenth
  df <- data.frame(
    date = as.Date(snotel_data$date),
    snow_depth_in = round(snotel_data$snow_depth / 25.4, 1),  # Convert mm to inches
    swe_in = round(snotel_data$snow_water_equivalent / 25.4, 1),  # Convert mm to inches
    precip_increment_in = round(snotel_data$precipitation / 25.4, 1),  # Daily precip in inches
    temp_max_f = round((snotel_data$temperature_max * 9/5) + 32, 1),  # Convert C to F
    temp_min_f = round((snotel_data$temperature_min * 9/5) + 32, 1)   # Convert C to F
  )

  # Calculate daily SWE increment (change from previous day)
  df <- df[order(df$date), ]
  df$swe_increment_in <- c(NA, round(diff(df$swe_in), 1))  # First day has no previous value

  # Calculate cumulative precipitation (water year starts Oct 1)
  df$water_year <- ifelse(format(df$date, "%m") >= "10",
                          as.numeric(format(df$date, "%Y")) + 1,
                          as.numeric(format(df$date, "%Y")))

  # Calculate cumulative precip for each water year
  df$precip_cumulative_in <- ave(df$precip_increment_in, df$water_year,
                                 FUN = function(x) round(cumsum(x), 1))

  # Remove water_year helper column
  df$water_year <- NULL

  # Reorder columns
  df <- df[, c("date", "snow_depth_in", "swe_in", "swe_increment_in",
               "precip_increment_in", "precip_cumulative_in",
               "temp_max_f", "temp_min_f")]

  # Filter by date range if specified
  if (!is.null(start_date)) {
    df <- df[df$date >= as.Date(start_date), ]
  }
  if (!is.null(end_date)) {
    df <- df[df$date <= as.Date(end_date), ]
  }

  # Adjust for start-of-period if specified
  if (period == "start") {
    # Shift dates forward by one day since SNOTEL reports end-of-day values
    df$date <- df$date + 1
    message("Note: Dates shifted forward by 1 day to represent start-of-period values")
  }

  # Create default filename if not provided
  if (is.null(output_file)) {
    if (!is.null(station_name)) {
      output_file <- paste0("snotel_", station_name, "_", station_id, ".csv")
    } else {
      output_file <- paste0("snotel_", station_id, ".csv")
    }
  }

  # Save to CSV
  write.csv(df, output_file, row.names = FALSE)

  # Print confirmation
  cat("Data saved to:", output_file, "\n")
  cat("Rows:", nrow(df), "\n")
  cat("Date range:", min(df$date), "to", max(df$date), "\n")

  # Return the data frame
  return(df)
}
