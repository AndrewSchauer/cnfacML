#' Identify Avalanche Seasons from Dates
#'
#' Assigns each date to an avalanche season. Avalanche season runs from November
#' to April/May, with the season year being the year of the November start date.
#'
#' @param dates A vector of dates. Can be either Date objects or character strings
#'   in MM/DD/YYYY format.
#'
#' @return An integer vector of season years, where each value represents the year
#'   in which the avalanche season started (November year). For example, a date in
#'   January 2024 would be assigned to season 2023 (representing the 2023/2024 season).
#'
#' @details
#' The function uses the following logic:
#' \itemize{
#'   \item If month is November or December, season year = calendar year
#'   \item If month is January through May, season year = calendar year - 1
#' }
#'
#' @examples
#' \dontrun{
#' # Using Date objects
#' dates <- as.Date(c("2023-12-15", "2024-01-20", "2024-11-10"))
#' identify_avalanche_seasons(dates)
#' # Returns: c(2023, 2023, 2024)
#'
#' # Using character strings (MM/DD/YYYY format)
#' dates <- c("12/15/2023", "01/20/2024", "11/10/2024")
#' identify_avalanche_seasons(dates)
#' # Returns: c(2023, 2023, 2024)
#' }
#'
#' @importFrom lubridate mdy year month
#' @export
identify_avalanche_seasons <- function(dates) {
  
  # Parse dates if they're strings
  if (is.character(dates)) {
    dates <- lubridate::mdy(dates)  # assuming MM/DD/YYYY format
  }
  
  # Avalanche season: November through April/May
  # Assign each date to a season (year of November start)
  year <- lubridate::year(dates)
  month <- lubridate::month(dates)
  
  # If month is Nov or Dec, season is that year
  # If month is Jan-May, season is previous year
  season <- ifelse(month >= 11, year, year - 1)
  
  return(season)
}
