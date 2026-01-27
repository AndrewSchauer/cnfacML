library(shiny)
library(dplyr)
library(lubridate)

# ============================================================================
# SNOTEL FUNCTIONS
# ============================================================================

fetch_snotel_data <- function() {
  url <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/start_of_period/954:AK:SNTL%7Cid=%22%22%7Cname/CurrentWY,CurrentWYEnd/WTEQ::value,SNWD::value,PREC::value,TMAX::value,TMIN::value?fitToScreen=false"

  tryCatch({
    all_lines <- readLines(url, warn = FALSE)
    header_line <- grep("^Date,", all_lines)[1]

    if(is.na(header_line)) {
      stop("Could not find header row in CSV")
    }

    data <- read.csv(url, skip = header_line - 1, header = TRUE,
                     stringsAsFactors = FALSE, check.names = FALSE)
    names(data) <- trimws(names(data))
    names(data) <- gsub("\\s+", "_", names(data))

    date_col <- grep("Date", names(data), ignore.case = TRUE)[1]
    if(!is.na(date_col)) {
      data[[date_col]] <- as.Date(data[[date_col]], format = "%Y-%m-%d")
      names(data)[date_col] <- "Date"
    }

    data <- data[!is.na(data$Date), ]
    data <- data %>% arrange(desc(Date))

    return(data)
  }, error = function(e) {
    message("Error fetching SNOTEL data: ", e$message)
    return(NULL)
  })
}

calculate_nday_total <- function(data, value_col, n_days) {
  if(nrow(data) < n_days + 1) return(NA)
  current_value <- ifelse(is.na(data[[value_col]][1]), 0, data[[value_col]][1])
  past_value <- ifelse(is.na(data[[value_col]][n_days + 1]), 0, data[[value_col]][n_days + 1])
  max(0, current_value - past_value)
}

calculate_snotel_form_values <- function(data, nws_mid_data = NULL) {
  if(is.null(data) || nrow(data) == 0) {
    return(list(
      snow_depth = 0, swe_in = 0, swe_increment_in = 0,
      precip_increment_in = 0, precip_cumulative_in = 0,
      temp_max_CR = 0, temp_min_CR = 0, snow_depth_3day = 0,
      swe_increment_3day = 0, precip_increment_3day = 0,
      snow_depth_7day = 0, swe_increment_7day = 0,
      precip_increment_7day = 0, snow_depth_increment = 0
    ))
  }

  all_cols <- names(data)
  swe_col <- all_cols[grep("WTEQ|Snow.*Water|SWE", all_cols, ignore.case = TRUE)][1]
  snow_depth_col <- all_cols[grep("SNWD|Snow.*Depth", all_cols, ignore.case = TRUE)][1]
  precip_col <- all_cols[grep("PREC|Precipitation|Precip\\.", all_cols, ignore.case = TRUE)][1]
  temp_max_col <- all_cols[grep("TMAX|Max.*Temp|Temperature.*Max", all_cols, ignore.case = TRUE)][1]
  temp_min_col <- all_cols[grep("TMIN|Min.*Temp|Temperature.*Min", all_cols, ignore.case = TRUE)][1]

  current_snow_depth <- ifelse(is.na(data[[snow_depth_col]][1]), 0, data[[snow_depth_col]][1])
  current_swe <- ifelse(is.na(data[[swe_col]][1]), 0, data[[swe_col]][1])
  precip_cumulative <- ifelse(is.na(data[[precip_col]][1]), 0, data[[precip_col]][1])
  
  # Use NWS Mid forecasted temps if available, otherwise fall back to SNOTEL
  temp_max <- 32
  temp_min <- 32
  
  if(!is.null(nws_mid_data)) {
    # Get the forecasted temps from NWS Mid data attributes
    nws_temps <- attr(nws_mid_data, "temps")
    nws_min_max <- attr(nws_mid_data, "min_max")
    
    # Use Min/Max Temp values if available
    valid_min_max <- nws_min_max[!is.na(nws_min_max)]
    if(length(valid_min_max) >= 2) {
      temp_min <- valid_min_max[1]
      temp_max <- valid_min_max[2]
    } else if(length(valid_min_max) == 1) {
      temp_min <- valid_min_max[1]
      temp_max <- valid_min_max[1]
    } else if(!is.null(nws_temps)) {
      # Fall back to min/max of temperature values
      valid_temps <- nws_temps[!is.na(nws_temps)]
      if(length(valid_temps) > 0) {
        temp_min <- min(valid_temps)
        temp_max <- max(valid_temps)
      }
    }
  } else {
    # Fall back to SNOTEL historical temps
    temp_max <- if(!is.na(temp_max_col)) ifelse(is.na(data[[temp_max_col]][1]), 32, data[[temp_max_col]][1]) else 32
    temp_min <- if(!is.na(temp_min_col)) ifelse(is.na(data[[temp_min_col]][1]), 32, data[[temp_min_col]][1]) else 32
  }

  # Calculate 24-h forecast values from NWS Mid Elevations (columns T1-T4)
  snow_24h_forecast <- 0
  swe_24h_forecast <- 0
  precip_24h_forecast <- 0
  
  if(!is.null(nws_mid_data)) {
    field_col <- which(names(nws_mid_data) == "Field")
    if(length(field_col) > 0) {
      # Find the relevant rows
      snow_6h_row <- which(nws_mid_data[[field_col]] == "6 Hour Snow")
      qpf_6h_row <- which(nws_mid_data[[field_col]] == "6 Hour QPF")
      
      # Sum values from columns 2-5 (Col1_3p, Col2_9p, Col3_3a, Col4_9a - first 24 hours)
      if(length(snow_6h_row) > 0) {
        for(col_idx in 2:5) {
          val <- suppressWarnings(as.numeric(nws_mid_data[snow_6h_row, col_idx]))
          if(!is.na(val)) snow_24h_forecast <- snow_24h_forecast + val
        }
      }
      
      if(length(qpf_6h_row) > 0) {
        for(col_idx in 2:5) {
          val <- suppressWarnings(as.numeric(nws_mid_data[qpf_6h_row, col_idx]))
          if(!is.na(val)) {
            precip_24h_forecast <- precip_24h_forecast + val
            # SWE is approximated as QPF (liquid equivalent)
            swe_24h_forecast <- swe_24h_forecast + val
          }
        }
      }
    }
  }
  
  # Get historical 2-day and 6-day incremental values from SNOTEL
  # For 3-day total: 24-h forecast + previous 2 days historical
  # For 7-day total: 24-h forecast + previous 6 days historical
  
  # Calculate 2-day historical (days 2 and 3)
  swe_2day_hist <- 0
  precip_2day_hist <- 0
  snow_2day_hist <- 0
  
  if(nrow(data) >= 3) {
    # Day 2 increment
    swe_day2 <- max(0, data[[swe_col]][2] - data[[swe_col]][3], na.rm = TRUE)
    precip_day2 <- max(0, data[[precip_col]][2] - data[[precip_col]][3], na.rm = TRUE)
    snow_day2 <- max(0, data[[snow_depth_col]][2] - data[[snow_depth_col]][3], na.rm = TRUE)
    
    swe_2day_hist <- swe_day2
    precip_2day_hist <- precip_day2
    snow_2day_hist <- snow_day2
  }
  
  if(nrow(data) >= 4) {
    # Day 3 increment
    swe_day3 <- max(0, data[[swe_col]][3] - data[[swe_col]][4], na.rm = TRUE)
    precip_day3 <- max(0, data[[precip_col]][3] - data[[precip_col]][4], na.rm = TRUE)
    snow_day3 <- max(0, data[[snow_depth_col]][3] - data[[snow_depth_col]][4], na.rm = TRUE)
    
    swe_2day_hist <- swe_2day_hist + swe_day3
    precip_2day_hist <- precip_2day_hist + precip_day3
    snow_2day_hist <- snow_2day_hist + snow_day3
  }
  
  # Calculate 6-day historical (days 2 through 7)
  swe_6day_hist <- swe_2day_hist
  precip_6day_hist <- precip_2day_hist
  snow_6day_hist <- snow_2day_hist
  
  for(day in 4:7) {
    if(nrow(data) >= day + 1) {
      swe_increment <- max(0, data[[swe_col]][day] - data[[swe_col]][day + 1], na.rm = TRUE)
      precip_increment <- max(0, data[[precip_col]][day] - data[[precip_col]][day + 1], na.rm = TRUE)
      snow_increment <- max(0, data[[snow_depth_col]][day] - data[[snow_depth_col]][day + 1], na.rm = TRUE)
      
      swe_6day_hist <- swe_6day_hist + swe_increment
      precip_6day_hist <- precip_6day_hist + precip_increment
      snow_6day_hist <- snow_6day_hist + snow_increment
    }
  }
  
  # Calculate totals
  swe_3day <- swe_24h_forecast + swe_2day_hist
  precip_3day <- precip_24h_forecast + precip_2day_hist
  snow_3day <- snow_24h_forecast + snow_2day_hist
  
  swe_7day <- swe_24h_forecast + swe_6day_hist
  precip_7day <- precip_24h_forecast + precip_6day_hist
  snow_7day <- snow_24h_forecast + snow_6day_hist

  list(
    snow_depth = round(current_snow_depth, 2),
    swe_in = round(current_swe, 2),
    swe_increment_in = round(swe_24h_forecast, 2),
    precip_increment_in = round(precip_24h_forecast, 2),
    precip_cumulative_in = round(precip_cumulative, 2),
    temp_max_CR = round(temp_max, 1),
    temp_min_CR = round(temp_min, 1),
    snow_depth_3day = round(snow_3day, 2),
    swe_increment_3day = round(swe_3day, 2),
    precip_increment_3day = round(precip_3day, 2),
    snow_depth_7day = round(snow_7day, 2),
    swe_increment_7day = round(swe_7day, 2),
    precip_increment_7day = round(precip_7day, 2),
    snow_depth_increment = round(snow_24h_forecast, 2)
  )
}

# ============================================================================
# NWS FUNCTIONS
# ============================================================================

fetch_nws_data <- function(section_name = "Turnagain Pass Upper Elevations") {
  url <- "https://forecast.weather.gov/product.php?site=afc&issuedby=afc&product=AVG&format=txt&version=1&glossary=0"
  
  tryCatch({
    lines <- readLines(url, warn = FALSE)
    section_start <- grep(section_name, lines, ignore.case = TRUE)[1]
    
    if(is.na(section_start)) {
      stop(paste("Could not find", section_name, "section"))
    }
    
    cat("\n=== Parsing", section_name, "===\n")
    
    header_line <- grep("^Time \\(LT\\)", lines[section_start:length(lines)])[1] + section_start - 1
    
    if(is.na(header_line)) {
      stop("Could not find Time (LT) header")
    }
    
    data_lines <- lines[(header_line + 3):length(lines)]
    next_section <- grep("^\\.\\.\\.", data_lines)[1]
    if(!is.na(next_section)) {
      data_lines <- data_lines[1:(next_section - 1)]
    }
    
    # Extract time labels
    time_line <- lines[header_line + 1]
    time_ampm_line <- lines[header_line + 2]
    
    cat("DEBUG - Time line:", time_line, "\n")
    cat("DEBUG - Time ampm line:", time_ampm_line, "\n")
    
    # The time labels (3p, 9p, etc.) are on the first line, not the second
    # Remove the "Time (LT)" label and parse the rest
    time_str <- sub("^.*\\(LT\\)\\s+", "", time_line)
    time_str <- trimws(time_str)
    cat("DEBUG - Cleaned time string:", time_str, "\n")
    
    time_labels_raw <- strsplit(time_str, "\\s+")[[1]]
    
    cat("DEBUG - Extracted time_labels_raw:", paste(time_labels_raw, collapse=", "), "\n")
    cat("DEBUG - Length of time_labels_raw:", length(time_labels_raw), "\n")
    
    time_labels <- character(9)
    for(i in 1:9) {
      if(i <= length(time_labels_raw)) {
        time_labels[i] <- paste0("Col", i, "_", time_labels_raw[i])
      } else {
        time_labels[i] <- paste0("T", i)
      }
    }
    
    if(length(time_labels) < 9) {
      time_labels <- c(time_labels, paste0("T", (length(time_labels)+1):9))
    }
    
    cat("Time labels found:", time_labels, "\n")
    
    # Parser functions
    parse_fixed_width_line <- function(line, is_numeric = TRUE) {
      if(is.na(line) || length(line) == 0) return(rep(NA, 9))
      label_end <- regexpr("\\s{2,}", line)[1]
      if(label_end < 0) return(rep(NA, 9))
      data_part <- substr(line, label_end, nchar(line))
      values <- strsplit(trimws(data_part), "\\s+")[[1]]
      if(is_numeric) values <- suppressWarnings(as.numeric(values))
      if(length(values) < 9) values <- c(values, rep(NA, 9 - length(values)))
      else if(length(values) > 9) values <- values[1:9]
      return(values)
    }
    
    parse_sparse_line <- function(line) {
      if(is.na(line) || length(line) == 0) return(rep(NA, 9))
      result <- rep(NA, 9)
      match_pos <- regexpr("\\s{2,}[0-9]", line)
      if(match_pos > 0) {
        data_start <- match_pos + attr(match_pos, "match.length") - 1
        remaining <- substr(line, data_start, nchar(line))
        values <- as.numeric(unlist(strsplit(trimws(remaining), "\\s+")))
        value_positions <- gregexpr("[0-9.]+", remaining)[[1]]
        col_width <- 6
        for(j in seq_along(values)) {
          if(j <= length(value_positions)) {
            pos <- value_positions[j]
            col_num <- ceiling(pos / col_width)
            if(col_num >= 1 && col_num <= 9) {
              result[col_num] <- values[j]
            }
          }
        }
      }
      return(result)
    }
    
    # Field definitions
    field_patterns <- list(
      "Cloud Cover[^%]", "Cloud Cover \\(%\\)", "Temperature", "Min/Max Temp",
      "Wind Dir", "Wind \\(mph\\)", "Wind Gust \\(mph\\)", "Precip Prob \\(%\\)",
      "Precip Type", "6 Hour QPF", "6 Hour Snow", "12 Hour Snow", "Snow Level \\(kft\\)"
    )
    
    field_names <- c(
      "Cloud Cover", "Cloud Cover (%)", "Temperature", "Min/Max Temp",
      "Wind Dir", "Wind (mph)", "Wind Gust (mph)", "Precip Prob (%)",
      "Precip Type", "6 Hour QPF", "6 Hour Snow", "12 Hour Snow", "Snow Level (kft)"
    )
    
    # Extract all data rows
    all_data <- list()
    for(i in seq_along(field_patterns)) {
      line <- grep(paste0("^", field_patterns[i]), data_lines, value = TRUE)[1]
      if(!is.na(line)) {
        if(field_names[i] == "Min/Max Temp" || field_names[i] == "12 Hour Snow") {
          all_data[[field_names[i]]] <- parse_sparse_line(line)
        } else {
          is_numeric <- !(field_names[i] %in% c("Cloud Cover", "Wind Dir", "Precip Type"))
          all_data[[field_names[i]]] <- parse_fixed_width_line(line, is_numeric)
        }
      } else {
        all_data[[field_names[i]]] <- rep(NA, 9)
      }
    }
    
    # Create display dataframe
    data <- data.frame(Field = c("Time", field_names), stringsAsFactors = FALSE)
    
    for(i in 1:9) {
      col_name <- time_labels[i]
      display_time <- if(i <= length(time_labels_raw)) time_labels_raw[i] else ""
      col_values <- c(display_time)
      
      for(field in field_names) {
        value <- all_data[[field]][i]
        if(is.na(value)) {
          col_values <- c(col_values, "")
        } else if(field %in% c("Cloud Cover", "Wind Dir", "Precip Type")) {
          col_values <- c(col_values, as.character(value))
        } else {
          col_values <- c(col_values, as.character(value))
        }
      }
      
      data[[col_name]] <- col_values
    }
    
    cat("Created table with", ncol(data), "columns (Field +", ncol(data)-1, "time periods)\n")
    
    # Store parsed numeric data for calculations
    attr(data, "temps") <- all_data[["Temperature"]][1:5]
    attr(data, "wind_dirs") <- all_data[["Wind Dir"]][1:5]
    attr(data, "wind_speeds") <- all_data[["Wind (mph)"]][1:5]
    attr(data, "wind_gusts") <- all_data[["Wind Gust (mph)"]][1:5]
    attr(data, "min_max") <- all_data[["Min/Max Temp"]]
    attr(data, "time_labels_raw") <- time_labels_raw  # Store raw time labels for display
    
    cat("Parsed forecast data successfully\n")
    
    return(data)
    
  }, error = function(e) {
    message("Error fetching NWS data: ", e$message)
    return(NULL)
  })
}

calculate_nws_form_values <- function(data) {
  if(is.null(data) || nrow(data) == 0) {
    return(list(
      wind_avg_SB = 0, wind_max_SB = 0, dir_avg_SB = "N",
      temp_min_SB = 32, temp_max_SB = 32
    ))
  }
  
  temps <- attr(data, "temps")
  wind_dirs <- attr(data, "wind_dirs")
  wind_speeds <- attr(data, "wind_speeds")
  wind_gusts <- attr(data, "wind_gusts")
  min_max <- attr(data, "min_max")
  
  # Wind calculations
  valid_speeds <- wind_speeds[!is.na(wind_speeds)]
  wind_avg <- if(length(valid_speeds) > 0) mean(valid_speeds) else 0
  
  valid_gusts <- wind_gusts[!is.na(wind_gusts)]
  wind_max <- if(length(valid_gusts) > 0) max(valid_gusts) else 0
  
  # Wind direction (most common)
  valid_dirs <- wind_dirs[!is.na(wind_dirs)]
  if(length(valid_dirs) > 0) {
    dir_string <- names(sort(table(valid_dirs), decreasing = TRUE))[1]
  } else {
    dir_string <- "N"
  }
  
  # Temperature
  valid_min_max <- min_max[!is.na(min_max)]
  if(length(valid_min_max) >= 2) {
    temp_min <- valid_min_max[1]
    temp_max <- valid_min_max[2]
  } else if(length(valid_min_max) == 1) {
    temp_min <- valid_min_max[1]
    temp_max <- valid_min_max[1]
  } else {
    valid_temps <- temps[!is.na(temps)]
    temp_min <- if(length(valid_temps) > 0) min(valid_temps) else 32
    temp_max <- if(length(valid_temps) > 0) max(valid_temps) else 32
  }
  
  list(
    wind_avg_SB = round(wind_avg, 1),
    wind_max_SB = round(wind_max, 1),
    dir_avg_SB = dir_string,
    temp_min_SB = round(temp_min, 1),
    temp_max_SB = round(temp_max, 1)
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Combined Weather Data Form - SNOTEL & NWS"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Fetch Controls"),
      actionButton("fetch_all", "Fetch All Data",
                   class = "btn-primary",
                   style = "width: 100%; margin-bottom: 10px;"),
      actionButton("fetch_snotel", "Fetch SNOTEL Only",
                   class = "btn-info",
                   style = "width: 100%; margin-bottom: 10px;"),
      actionButton("fetch_nws", "Fetch NWS Only",
                   class = "btn-info",
                   style = "width: 100%; margin-bottom: 10px;"),
      actionButton("reset_form", "Reset Form",
                   class = "btn-warning",
                   style = "width: 100%; margin-bottom: 20px;"),
      hr(),
      h4("Data Status:"),
      textOutput("data_status"),
      hr(),
      downloadButton("download_data", "Download All as CSV",
                     style = "width: 100%;")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # Combined Form Tab
        tabPanel("Form",
          h3("All Form Fields"),
          
          h4("Avalanche Problems"),
          fluidRow(
            column(4,
              selectInput("p1_used", "Problem 1 Type:",
                         choices = c("none" = "none", "Cornice" = "C", "Dry Loose" = "DL", 
                                   "Deep Slab" = "DS", "Glide" = "G", "Persistent Slab" = "PS",
                                   "Storm Slab" = "SS", "Wet Loose" = "WL", 
                                   "Wind Slab" = "WdS", "Wet Slab" = "WtS", "Spring" = "spring"),
                         selected = "none"),
              selectInput("p1_like", "Problem 1 Likelihood:",
                         choices = c("Unlikely" = "1", "Possible" = "2", "Likely" = "3", 
                                   "Very Likely" = "4", "Almost Certain" = "5"),
                         selected = "1"),
              numericInput("p1_minSize", "Problem 1 Min D-Size:", value = 0, min = 0, max = 5, step = 0.5),
              numericInput("p1_maxSize", "Problem 1 Max D-Size:", value = 0, min = 0, max = 5, step = 0.5)
            ),
            column(4,
              selectInput("p2_used", "Problem 2 Type:",
                         choices = c("none" = "none", "Cornice" = "C", "Dry Loose" = "DL", 
                                   "Deep Slab" = "DS", "Glide" = "G", "Persistent Slab" = "PS",
                                   "Storm Slab" = "SS", "Wet Loose" = "WL", 
                                   "Wind Slab" = "WdS", "Wet Slab" = "WtS"),
                         selected = "none"),
              selectInput("p2_like", "Problem 2 Likelihood:",
                         choices = c("Unlikely" = "1", "Possible" = "2", "Likely" = "3", 
                                   "Very Likely" = "4", "Almost Certain" = "5"),
                         selected = "1"),
              numericInput("p2_minSize", "Problem 2 Min D-Size:", value = 0, min = 0, max = 5, step = 0.5),
              numericInput("p2_maxSize", "Problem 2 Max D-Size:", value = 0, min = 0, max = 5, step = 0.5)
            ),
            column(4,
              selectInput("p3_used", "Problem 3 Type:",
                         choices = c("none" = "none", "Cornice" = "C", "Dry Loose" = "DL", 
                                   "Deep Slab" = "DS", "Glide" = "G", "Persistent Slab" = "PS",
                                   "Storm Slab" = "SS", "Wet Loose" = "WL", 
                                   "Wind Slab" = "WdS", "Wet Slab" = "WtS"),
                         selected = "none"),
              selectInput("p3_like", "Problem 3 Likelihood:",
                         choices = c("Unlikely" = "1", "Possible" = "2", "Likely" = "3", 
                                   "Very Likely" = "4", "Almost Certain" = "5"),
                         selected = "1"),
              numericInput("p3_minSize", "Problem 3 Min D-Size:", value = 0, min = 0, max = 5, step = 0.5),
              numericInput("p3_maxSize", "Problem 3 Max D-Size:", value = 0, min = 0, max = 5, step = 0.5)
            )
          ),
          
          hr(),
          
          h4("SNOTEL Station 954 (Alaska)"),
          fluidRow(
            column(4,
              numericInput("snow_depth", "Cumulative Seasonal Snow Depth (in):", value = 0, step = 0.1),
              numericInput("swe_in", "Cumulative Seasonal SWE (in):", value = 0, step = 0.1),
              numericInput("swe_increment_in", "24-hour Incremental SWE (in):", value = 0, step = 0.1),
              numericInput("precip_increment_in", "24-h Incremental Precip (in):", value = 0, step = 0.1),
              numericInput("precip_cumulative_in", "Seasonal Cumulative Precip (in):", value = 0, step = 0.1)
            ),
            column(4,
              numericInput("temp_max_CR", "Max Daily Temp at Center Ridge (°F):", value = 32, step = 0.1),
              numericInput("temp_min_CR", "Min Daily Temp at Center Ridge (°F):", value = 32, step = 0.1),
              numericInput("snow_depth_3day", "3-day Snow Total (in):", value = 0, step = 0.1),
              numericInput("swe_increment_3day", "3-day SWE Total (in):", value = 0, step = 0.1),
              numericInput("precip_increment_3day", "3-day Precip Total (in):", value = 0, step = 0.1)
            ),
            column(4,
              numericInput("snow_depth_7day", "7-day Snow Total (in):", value = 0, step = 0.1),
              numericInput("swe_increment_7day", "7-day SWE Total (in):", value = 0, step = 0.1),
              numericInput("precip_increment_7day", "7-day Precip Total (in):", value = 0, step = 0.1),
              numericInput("snow_depth_increment", "24-h Snow Total (in):", value = 0, step = 0.1)
            )
          ),
          
          hr(),
          
          h4("NWS Turnagain Pass - Upper Elevations (above 3000 ft)"),
          fluidRow(
            column(4,
              numericInput("wind_avg_hi", "Daily Average Wind Speed (mph):", value = 0, step = 0.1)
            ),
            column(4,
              numericInput("wind_max_hi", "Daily Max Wind Gust (mph):", value = 0, step = 0.1)
            ),
            column(4,
              selectInput("dir_avg_hi", "Average Wind Direction:",
                         choices = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                         selected = "N")
            )
          ),
          fluidRow(
            column(6,
              numericInput("temp_min_hi", "Daily Min Temperature (°F):", value = 32, step = 0.1)
            ),
            column(6,
              numericInput("temp_max_hi", "Daily Max Temperature (°F):", value = 32, step = 0.1)
            )
          ),
          
          hr(),
          
          h4("NWS Turnagain Pass - Mid Elevations (1500 to 3000 ft)"),
          fluidRow(
            column(4,
              numericInput("wind_avg_mid", "Daily Average Wind Speed (mph):", value = 0, step = 0.1)
            ),
            column(4,
              numericInput("wind_max_mid", "Daily Max Wind Gust (mph):", value = 0, step = 0.1)
            ),
            column(4,
              selectInput("dir_avg_mid", "Average Wind Direction:",
                         choices = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                         selected = "N")
            )
          ),
          fluidRow(
            column(6,
              numericInput("temp_min_mid", "Daily Min Temperature (°F):", value = 32, step = 0.1)
            ),
            column(6,
              numericInput("temp_max_mid", "Daily Max Temperature (°F):", value = 32, step = 0.1)
            )
          )
        ),
        
        # SNOTEL Data Tab
        tabPanel("SNOTEL Data",
          h4("Raw Data (Last 7 Days)"),
          tableOutput("snotel_data_table"),
          hr(),
          h4("30-Day Trends"),
          fluidRow(
            column(6, plotOutput("snow_depth_plot", height = "300px")),
            column(6, plotOutput("swe_plot", height = "300px"))
          ),
          fluidRow(
            column(6, plotOutput("precip_plot", height = "300px")),
            column(6, plotOutput("temp_plot", height = "300px"))
          )
        ),
        
        # NWS Data Tab
        tabPanel("NWS Tables",
          h4("Turnagain Pass Upper Elevations (above 3000 ft)"),
          p("(Form fields use first 5 columns - 24-hour period)"),
          div(style = "overflow-x: auto;", tableOutput("nws_upper_table")),
          hr(),
          h4("Turnagain Pass Mid Elevations (1500 to 3000 ft)"),
          p("(Form fields use first 5 columns - 24-hour period)"),
          div(style = "overflow-x: auto;", tableOutput("nws_mid_table"))
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  snotel_data <- reactiveVal(NULL)
  nws_data_upper <- reactiveVal(NULL)
  nws_data_mid <- reactiveVal(NULL)
  
  # Fetch all data
  observeEvent(input$fetch_all, {
    showModal(modalDialog(title = "Fetching Data", "Please wait...", footer = NULL))
    
    # Fetch NWS Mid first (needed for SNOTEL calculations)
    nws_mid <- fetch_nws_data("Turnagain Pass Mid Elevations")
    nws_data_mid(nws_mid)
    
    # Fetch SNOTEL
    snotel <- fetch_snotel_data()
    snotel_data(snotel)
    if(!is.null(snotel)) {
      values <- calculate_snotel_form_values(snotel, nws_mid)
      updateNumericInput(session, "snow_depth", value = values$snow_depth)
      updateNumericInput(session, "swe_in", value = values$swe_in)
      updateNumericInput(session, "swe_increment_in", value = values$swe_increment_in)
      updateNumericInput(session, "precip_increment_in", value = values$precip_increment_in)
      updateNumericInput(session, "precip_cumulative_in", value = values$precip_cumulative_in)
      updateNumericInput(session, "temp_max_CR", value = values$temp_max_CR)
      updateNumericInput(session, "temp_min_CR", value = values$temp_min_CR)
      updateNumericInput(session, "snow_depth_3day", value = values$snow_depth_3day)
      updateNumericInput(session, "swe_increment_3day", value = values$swe_increment_3day)
      updateNumericInput(session, "precip_increment_3day", value = values$precip_increment_3day)
      updateNumericInput(session, "snow_depth_7day", value = values$snow_depth_7day)
      updateNumericInput(session, "swe_increment_7day", value = values$swe_increment_7day)
      updateNumericInput(session, "precip_increment_7day", value = values$precip_increment_7day)
      updateNumericInput(session, "snow_depth_increment", value = values$snow_depth_increment)
    }
    
    # Fetch NWS Upper
    nws_upper <- fetch_nws_data("Turnagain Pass Upper Elevations")
    nws_data_upper(nws_upper)
    if(!is.null(nws_upper)) {
      values <- calculate_nws_form_values(nws_upper)
      updateNumericInput(session, "wind_avg_hi", value = values$wind_avg_SB)
      updateNumericInput(session, "wind_max_hi", value = values$wind_max_SB)
      updateSelectInput(session, "dir_avg_hi", selected = values$dir_avg_SB)
      updateNumericInput(session, "temp_min_hi", value = values$temp_min_SB)
      updateNumericInput(session, "temp_max_hi", value = values$temp_max_SB)
    }
    
    # Update NWS Mid form values
    if(!is.null(nws_mid)) {
      values <- calculate_nws_form_values(nws_mid)
      updateNumericInput(session, "wind_avg_mid", value = values$wind_avg_SB)
      updateNumericInput(session, "wind_max_mid", value = values$wind_max_SB)
      updateSelectInput(session, "dir_avg_mid", selected = values$dir_avg_SB)
      updateNumericInput(session, "temp_min_mid", value = values$temp_min_SB)
      updateNumericInput(session, "temp_max_mid", value = values$temp_max_SB)
    }
    
    removeModal()
  })
  
  # Fetch SNOTEL only
  observeEvent(input$fetch_snotel, {
    showModal(modalDialog(title = "Fetching SNOTEL Data", "Please wait...", footer = NULL))
    
    # Also fetch NWS Mid to calculate forecast-based increments
    nws_mid <- fetch_nws_data("Turnagain Pass Mid Elevations")
    nws_data_mid(nws_mid)
    
    snotel <- fetch_snotel_data()
    snotel_data(snotel)
    if(!is.null(snotel)) {
      values <- calculate_snotel_form_values(snotel, nws_mid)
      updateNumericInput(session, "snow_depth", value = values$snow_depth)
      updateNumericInput(session, "swe_in", value = values$swe_in)
      updateNumericInput(session, "swe_increment_in", value = values$swe_increment_in)
      updateNumericInput(session, "precip_increment_in", value = values$precip_increment_in)
      updateNumericInput(session, "precip_cumulative_in", value = values$precip_cumulative_in)
      updateNumericInput(session, "temp_max_CR", value = values$temp_max_CR)
      updateNumericInput(session, "temp_min_CR", value = values$temp_min_CR)
      updateNumericInput(session, "snow_depth_3day", value = values$snow_depth_3day)
      updateNumericInput(session, "swe_increment_3day", value = values$swe_increment_3day)
      updateNumericInput(session, "precip_increment_3day", value = values$precip_increment_3day)
      updateNumericInput(session, "snow_depth_7day", value = values$snow_depth_7day)
      updateNumericInput(session, "swe_increment_7day", value = values$swe_increment_7day)
      updateNumericInput(session, "precip_increment_7day", value = values$precip_increment_7day)
      updateNumericInput(session, "snow_depth_increment", value = values$snow_depth_increment)
    }
    
    # Also update NWS Mid form values since we fetched it
    if(!is.null(nws_mid)) {
      values <- calculate_nws_form_values(nws_mid)
      updateNumericInput(session, "wind_avg_mid", value = values$wind_avg_SB)
      updateNumericInput(session, "wind_max_mid", value = values$wind_max_SB)
      updateSelectInput(session, "dir_avg_mid", selected = values$dir_avg_SB)
      updateNumericInput(session, "temp_min_mid", value = values$temp_min_SB)
      updateNumericInput(session, "temp_max_mid", value = values$temp_max_SB)
    }
    
    removeModal()
  })
  
  # Fetch NWS only
  observeEvent(input$fetch_nws, {
    showModal(modalDialog(title = "Fetching NWS Data", "Please wait...", footer = NULL))
    
    nws_upper <- fetch_nws_data("Turnagain Pass Upper Elevations")
    nws_data_upper(nws_upper)
    if(!is.null(nws_upper)) {
      values <- calculate_nws_form_values(nws_upper)
      updateNumericInput(session, "wind_avg_hi", value = values$wind_avg_SB)
      updateNumericInput(session, "wind_max_hi", value = values$wind_max_SB)
      updateSelectInput(session, "dir_avg_hi", selected = values$dir_avg_SB)
      updateNumericInput(session, "temp_min_hi", value = values$temp_min_SB)
      updateNumericInput(session, "temp_max_hi", value = values$temp_max_SB)
    }
    
    nws_mid <- fetch_nws_data("Turnagain Pass Mid Elevations")
    nws_data_mid(nws_mid)
    if(!is.null(nws_mid)) {
      values <- calculate_nws_form_values(nws_mid)
      updateNumericInput(session, "wind_avg_mid", value = values$wind_avg_SB)
      updateNumericInput(session, "wind_max_mid", value = values$wind_max_SB)
      updateSelectInput(session, "dir_avg_mid", selected = values$dir_avg_SB)
      updateNumericInput(session, "temp_min_mid", value = values$temp_min_SB)
      updateNumericInput(session, "temp_max_mid", value = values$temp_max_SB)
    }
    
    removeModal()
  })
  
  # Reset form
  observeEvent(input$reset_form, {
    # Avalanche fields
    updateSelectInput(session, "p1_used", selected = "none")
    updateSelectInput(session, "p1_like", selected = "1")
    updateNumericInput(session, "p1_minSize", value = 0)
    updateNumericInput(session, "p1_maxSize", value = 0)
    updateSelectInput(session, "p2_used", selected = "none")
    updateSelectInput(session, "p2_like", selected = "1")
    updateNumericInput(session, "p2_minSize", value = 0)
    updateNumericInput(session, "p2_maxSize", value = 0)
    updateSelectInput(session, "p3_used", selected = "none")
    updateSelectInput(session, "p3_like", selected = "1")
    updateNumericInput(session, "p3_minSize", value = 0)
    updateNumericInput(session, "p3_maxSize", value = 0)
    
    # SNOTEL fields
    updateNumericInput(session, "snow_depth", value = 0)
    updateNumericInput(session, "swe_in", value = 0)
    updateNumericInput(session, "swe_increment_in", value = 0)
    updateNumericInput(session, "precip_increment_in", value = 0)
    updateNumericInput(session, "precip_cumulative_in", value = 0)
    updateNumericInput(session, "temp_max_CR", value = 32)
    updateNumericInput(session, "temp_min_CR", value = 32)
    updateNumericInput(session, "snow_depth_3day", value = 0)
    updateNumericInput(session, "swe_increment_3day", value = 0)
    updateNumericInput(session, "precip_increment_3day", value = 0)
    updateNumericInput(session, "snow_depth_7day", value = 0)
    updateNumericInput(session, "swe_increment_7day", value = 0)
    updateNumericInput(session, "precip_increment_7day", value = 0)
    updateNumericInput(session, "snow_depth_increment", value = 0)
    
    # NWS fields
    updateNumericInput(session, "wind_avg_hi", value = 0)
    updateNumericInput(session, "wind_max_hi", value = 0)
    updateSelectInput(session, "dir_avg_hi", selected = "N")
    updateNumericInput(session, "temp_min_hi", value = 32)
    updateNumericInput(session, "temp_max_hi", value = 32)
    updateNumericInput(session, "wind_avg_mid", value = 0)
    updateNumericInput(session, "wind_max_mid", value = 0)
    updateSelectInput(session, "dir_avg_mid", selected = "N")
    updateNumericInput(session, "temp_min_mid", value = 32)
    updateNumericInput(session, "temp_max_mid", value = 32)
  })
  
  # Data status
  output$data_status <- renderText({
    snotel <- snotel_data()
    nws_upper <- nws_data_upper()
    nws_mid <- nws_data_mid()
    
    status <- character()
    if(!is.null(snotel)) status <- c(status, "SNOTEL loaded")
    if(!is.null(nws_upper)) status <- c(status, "NWS Upper loaded")
    if(!is.null(nws_mid)) status <- c(status, "NWS Mid loaded")
    
    if(length(status) == 0) {
      "No data loaded. Click 'Fetch All Data' to begin."
    } else {
      paste(status, collapse = " | ")
    }
  })
  
  # SNOTEL data table
  output$snotel_data_table <- renderTable({
    data <- snotel_data()
    if(is.null(data)) return(data.frame(Message = "No data loaded yet"))
    display_data <- head(data, 7)
    if("Date" %in% names(display_data)) {
      display_data$Date <- format(display_data$Date, "%m/%d/%Y")
    }
    display_data
  })
  
  # NWS tables
  output$nws_upper_table <- renderTable({
    data <- nws_data_upper()
    if(is.null(data)) return(data.frame(Message = "No data loaded yet"))
    
    # Get the raw time labels from attributes
    time_labels_raw <- attr(data, "time_labels_raw")
    
    # Replace column names with the time labels
    new_names <- names(data)
    for(i in 2:length(new_names)) {
      # Use time_labels_raw for columns 2-10 (which correspond to time periods 1-9)
      if(i - 1 <= length(time_labels_raw)) {
        new_names[i] <- time_labels_raw[i - 1]
      }
    }
    names(data) <- new_names
    
    # Remove the "Time" row since we now have times as column headers
    data <- data[data$Field != "Time", ]
    
    data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  output$nws_mid_table <- renderTable({
    data <- nws_data_mid()
    if(is.null(data)) return(data.frame(Message = "No data loaded yet"))
    
    # Get the raw time labels from attributes
    time_labels_raw <- attr(data, "time_labels_raw")
    
    # Replace column names with the time labels
    new_names <- names(data)
    for(i in 2:length(new_names)) {
      if(i - 1 <= length(time_labels_raw)) {
        new_names[i] <- time_labels_raw[i - 1]
      }
    }
    names(data) <- new_names
    
    # Remove the "Time" row
    data <- data[data$Field != "Time", ]
    
    data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # SNOTEL plots
  output$snow_depth_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    snow_col <- grep("SNWD|Snow.*Depth", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(snow_col)) return(NULL)
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[snow_col]], type = "l", lwd = 2, col = "blue",
         xlab = "Date", ylab = "Snow Depth (inches)", main = "Snow Depth - Last 30 Days")
    grid()
  })
  
  output$swe_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    swe_col <- grep("WTEQ|Snow.*Water|SWE", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(swe_col)) return(NULL)
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[swe_col]], type = "l", lwd = 2, col = "darkblue",
         xlab = "Date", ylab = "SWE (inches)", main = "Snow Water Equivalent - Last 30 Days")
    grid()
  })
  
  output$precip_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    precip_col <- grep("PREC|Precipitation|Precip\\.", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(precip_col)) return(NULL)
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[precip_col]], type = "l", lwd = 2, col = "green4",
         xlab = "Date", ylab = "Cumulative Precipitation (inches)", main = "Precipitation - Last 30 Days")
    grid()
  })
  
  output$temp_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    temp_max_col <- grep("TMAX|Max.*Temp|Temperature.*Max", names(data), value = TRUE, ignore.case = TRUE)[1]
    temp_min_col <- grep("TMIN|Min.*Temp|Temperature.*Min", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(temp_max_col) && is.na(temp_min_col)) return(NULL)
    plot_data <- head(data, 30)
    y_range <- range(c(plot_data[[temp_max_col]], plot_data[[temp_min_col]]), na.rm = TRUE)
    plot(plot_data$Date, plot_data[[temp_max_col]], type = "l", lwd = 2, col = "red",
         xlab = "Date", ylab = "Temperature (°F)", main = "Temperature - Last 30 Days", ylim = y_range)
    lines(plot_data$Date, plot_data[[temp_min_col]], lwd = 2, col = "blue")
    legend("topright", legend = c("Max Temp", "Min Temp"), col = c("red", "blue"), lwd = 2, bty = "n")
    grid()
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("combined_weather_form_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Convert wind directions to degrees
      dir_map <- c(N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5,
                   SE=135, SSE=157.5, S=180, SSW=202.5, SW=225, WSW=247.5,
                   W=270, WNW=292.5, NW=315, NNW=337.5)
      
      dir_hi_deg <- ifelse(input$dir_avg_hi %in% names(dir_map), dir_map[input$dir_avg_hi], 0)
      dir_mid_deg <- ifelse(input$dir_avg_mid %in% names(dir_map), dir_map[input$dir_avg_mid], 0)
      
      # Binary code problem types matching CSV structure: C, DL, DS, G, PS, SS, WL, WdS, WtS, none, spring
      problem_types <- c("C", "DL", "DS", "G", "PS", "SS", "WL", "WdS", "WtS", "none", "spring")
      
      # Initialize all binary columns to 0
      p1_binary <- setNames(rep(0, length(problem_types)), problem_types)
      p2_binary <- setNames(rep(0, length(problem_types)), problem_types)
      p3_binary <- setNames(rep(0, length(problem_types)), problem_types)
      
      # Set the selected problem type to 1
      if(input$p1_used %in% problem_types) {
        p1_binary[input$p1_used] <- 1
      }
      if(input$p2_used %in% problem_types) {
        p2_binary[input$p2_used] <- 1
      }
      if(input$p3_used %in% problem_types) {
        p3_binary[input$p3_used] <- 1
      }
      
      # Handle min/max D-size: if one is 0 and the other isn't, copy the non-zero value
      p1_min <- input$p1_minSize
      p1_max <- input$p1_maxSize
      if(p1_min == 0 && p1_max > 0) p1_min <- p1_max
      if(p1_max == 0 && p1_min > 0) p1_max <- p1_min
      
      p2_min <- input$p2_minSize
      p2_max <- input$p2_maxSize
      if(p2_min == 0 && p2_max > 0) p2_min <- p2_max
      if(p2_max == 0 && p2_min > 0) p2_max <- p2_min
      
      p3_min <- input$p3_minSize
      p3_max <- input$p3_maxSize
      if(p3_min == 0 && p3_max > 0) p3_min <- p3_max
      if(p3_max == 0 && p3_min > 0) p3_max <- p3_min
      
      # Create data frame matching exact CSV column order
      form_data <- data.frame(
        Date = format(Sys.Date(), "%m/%d/%Y"),
        alp.used = NA,  # Not collected in form
        tl.used = NA,   # Not collected in form
        btl.used = NA,  # Not collected in form
        p1.used = input$p1_used,
        p2.used = input$p2_used,
        p3.used = input$p3_used,
        p1.like = as.numeric(input$p1_like),  # Convert from text to numeric
        p1.minSize = p1_min,
        p1.maxSize = p1_max,
        p2.like = as.numeric(input$p2_like),  # Convert from text to numeric
        p2.minSize = p2_min,
        p2.maxSize = p2_max,
        p3.like = as.numeric(input$p3_like),  # Convert from text to numeric
        p3.minSize = p3_min,
        p3.maxSize = p3_max,
        snow_depth_in = input$snow_depth,
        swe_in = input$swe_in,
        swe_increment_in = input$swe_increment_in,
        precip_increment_in = input$precip_increment_in,
        precip_cumulative_in = input$precip_cumulative_in,
        temp_max_CR = input$temp_max_CR,
        temp_min_CR = input$temp_min_CR,
        snow_depth_3day = input$snow_depth_3day,
        swe_increment_3day = input$swe_increment_3day,
        precip_increment_3day = input$precip_increment_3day,
        snow_depth_7day = input$snow_depth_7day,
        swe_increment_7day = input$swe_increment_7day,
        precip_increment_7day = input$precip_increment_7day,
        snow_depth_increment = input$snow_depth_increment,
        wind_avg_hi = input$wind_avg_hi,
        wind_max_hi = input$wind_max_hi,
        dir_avg_hi = dir_hi_deg,
        temp_min_hi = input$temp_min_hi,
        temp_max_hi = input$temp_max_hi,
        wind_avg_mid = input$wind_avg_mid,
        wind_max_mid = input$wind_max_mid,
        dir_avg_mid = dir_mid_deg,
        temp_min_mid = input$temp_min_mid,
        temp_max_mid = input$temp_max_mid,
        # Problem 1 binary codes
        p1.C = p1_binary["C"],
        p1.DL = p1_binary["DL"],
        p1.DS = p1_binary["DS"],
        p1.G = p1_binary["G"],
        p1.PS = p1_binary["PS"],
        p1.SS = p1_binary["SS"],
        p1.WL = p1_binary["WL"],
        p1.WdS = p1_binary["WdS"],
        p1.WtS = p1_binary["WtS"],
        p1.none = p1_binary["none"],
        p1.spring = p1_binary["spring"],
        # Problem 2 binary codes
        p2.C = p2_binary["C"],
        p2.DL = p2_binary["DL"],
        p2.DS = p2_binary["DS"],
        p2.G = p2_binary["G"],
        p2.PS = p2_binary["PS"],
        p2.SS = p2_binary["SS"],
        p2.WL = p2_binary["WL"],
        p2.WdS = p2_binary["WdS"],
        p2.WtS = p2_binary["WtS"],
        p2.none = p2_binary["none"],
        # Problem 3 binary codes
        p3.C = p3_binary["C"],
        p3.DL = p3_binary["DL"],
        p3.DS = p3_binary["DS"],
        p3.G = p3_binary["G"],
        p3.PS = p3_binary["PS"],
        p3.SS = p3_binary["SS"],
        p3.WL = p3_binary["WL"],
        p3.WdS = p3_binary["WdS"],
        p3.WtS = p3_binary["WtS"],
        p3.none = p3_binary["none"]
      )
      
      write.csv(form_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
