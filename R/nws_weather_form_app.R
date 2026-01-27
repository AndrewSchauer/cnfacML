library(shiny)
library(dplyr)

# Function to fetch and parse NWS Avalanche Weather Guidance
fetch_nws_data <- function(section_name = "Turnagain Pass Upper Elevations") {
  url <- "https://forecast.weather.gov/product.php?site=afc&issuedby=afc&product=AVG&format=txt&version=1&glossary=0"
  
  tryCatch({
    # Read the page content
    lines <- readLines(url, warn = FALSE)
    
    # Find the specified section
    section_start <- grep(section_name, lines, ignore.case = TRUE)[1]
    
    if(is.na(section_start)) {
      stop(paste("Could not find", section_name, "section"))
    }
    
    cat("\n=== Parsing", section_name, "===\n")
    
    # Find where the data table starts
    header_line <- NA
    for(i in section_start:(section_start + 10)) {
      if(grepl("^Date", lines[i])) {
        header_line <- i
        break
      }
    }
    
    if(is.na(header_line)) {
      stop("Could not find data table header")
    }
    
    # Read lines after header to find the data rows we need
    # Skip Date, Time, separator lines
    data_start <- header_line + 4
    
    # Collect all data lines until we hit the next section or empty line
    data_lines <- list()
    current_line <- data_start
    
    while(current_line <= length(lines)) {
      line <- lines[current_line]
      # Stop if we hit the next section (starts with "...")
      if(grepl("^\\.\\.\\.", line)) break
      # Stop if empty line
      if(nchar(trimws(line)) == 0) break
      data_lines[[length(data_lines) + 1]] <- line
      current_line <- current_line + 1
    }
    
    # Extract time headers (all 9 columns)
    time_line <- lines[header_line + 1]
    time_ampm_line <- lines[header_line + 2]
    
    # Parse time line more carefully - it has values like "15    21    03    09    15    21    03    09    15"
    time_values_str <- trimws(gsub("^Time \\(LT\\)\\s+", "", time_line))
    time_values <- strsplit(time_values_str, "\\s+")[[1]]
    
    # Parse ampm line - it has values like "3p    9p    3a    9a    3p    9p    3a    9a    3p"
    time_ampm_str <- trimws(time_ampm_line)
    time_labels_raw <- strsplit(time_ampm_str, "\\s+")[[1]]
    
    # Make column names unique by adding sequential numbers
    # The raw labels repeat (3p, 9p, 3a, 9a, 3p, 9p, 3a, 9a, 3p)
    time_labels <- character(9)
    for(i in 1:9) {
      if(i <= length(time_labels_raw)) {
        # Make truly unique by adding the column number
        time_labels[i] <- paste0("Col", i, "_", time_labels_raw[i])
      } else {
        time_labels[i] <- paste0("T", i)
      }
    }
    
    # Make sure we have at least 9 labels
    if(length(time_labels) < 9) {
      time_labels <- c(time_labels, paste0("T", (length(time_labels)+1):9))
    }
    
    cat("Time labels found:", time_labels, "\n")
    
    # Helper function to extract values from fixed-width format
    parse_fixed_width_line <- function(line, is_numeric = TRUE) {
      if(is.na(line) || length(line) == 0) return(rep(NA, 9))
      
      # Remove the label part
      label_end <- regexpr("\\s{2,}", line)[1]
      if(label_end < 0) return(rep(NA, 9))
      
      data_part <- substr(line, label_end, nchar(line))
      
      # Split by whitespace
      values <- strsplit(trimws(data_part), "\\s+")[[1]]
      
      if(is_numeric) {
        values <- suppressWarnings(as.numeric(values))
      }
      
      # Ensure we have exactly 9 values
      if(length(values) < 9) {
        values <- c(values, rep(NA, 9 - length(values)))
      } else if(length(values) > 9) {
        values <- values[1:9]
      }
      
      return(values)
    }
    
    # Special parser for sparse rows - align with actual character positions in the line
    parse_sparse_line <- function(line) {
      if(is.na(line) || length(line) == 0) return(rep(NA, 9))
      
      result <- rep(NA, 9)
      
      # The issue: we need to know where the data columns START in the original line
      # Looking at a full data line like "Temperature        30    28    26..."
      # and comparing to "Min/Max Temp                   22          25..."
      # We need to find the column positions by looking at where Temperature values are
      
      # For now, let's use a reference: in the full line, after any field label,
      # the first data column starts at approximately position 19-23
      # Let's find the position after the label by looking for where numbers start
      
      # Method: The data portion starts after significant whitespace (2+ spaces)
      match_pos <- regexpr("\\s{2,}[0-9]", line)
      if(match_pos > 0) {
        # Found where data starts
        data_start <- match_pos + attr(match_pos, "match.length") - 1 # Position of first digit
        
        # Now extract from this position, treating each 6-char block as a column
        remaining <- substr(line, data_start, nchar(line))
        
        # But we need to back up to align with column boundaries
        # The first digit found is in column 1, 2, or 3
        # Let's extract values by looking at intervals
        col_width <- 6
        
        # Get all numeric values in order
        values <- as.numeric(unlist(strsplit(trimws(remaining), "\\s+")))
        
        # Distribute them in the result - but where?
        # Use the spacing to figure it out
        value_positions <- gregexpr("[0-9.]+", remaining)[[1]]
        
        for(j in seq_along(values)) {
          if(j <= length(value_positions)) {
            pos <- value_positions[j]
            # Figure out which column this belongs to (approximately)
            col_num <- ceiling(pos / col_width)
            if(col_num >= 1 && col_num <= 9) {
              result[col_num] <- values[j]
            }
          }
        }
      }
      
      return(result)
    }
    
    # Find and parse ALL rows from the table
    field_patterns <- list(
      "Cloud Cover[^%]",
      "Cloud Cover \\(%\\)",
      "Temperature",
      "Min/Max Temp",
      "Wind Dir",
      "Wind \\(mph\\)",
      "Wind Gust \\(mph\\)",
      "Precip Prob \\(%\\)",
      "Precip Type",
      "6 Hour QPF",
      "6 Hour Snow",
      "12 Hour Snow",
      "Snow Level \\(kft\\)"
    )
    
    field_names <- c(
      "Cloud Cover",
      "Cloud Cover (%)",
      "Temperature",
      "Min/Max Temp",
      "Wind Dir",
      "Wind (mph)",
      "Wind Gust (mph)",
      "Precip Prob (%)",
      "Precip Type",
      "6 Hour QPF",
      "6 Hour Snow",
      "12 Hour Snow",
      "Snow Level (kft)"
    )
    
    # Extract all data rows
    all_data <- list()
    for(i in seq_along(field_patterns)) {
      line <- grep(paste0("^", field_patterns[i]), data_lines, value = TRUE)[1]
      if(!is.na(line)) {
        if(field_names[i] == "Min/Max Temp") {
          # Special handling for Min/Max Temp - it has sparse values
          cat("Min/Max Temp line:", line, "\n")
          all_data[[field_names[i]]] <- parse_sparse_line(line)
          cat("Parsed Min/Max values:", all_data[[field_names[i]]], "\n")
        } else if(field_names[i] == "12 Hour Snow") {
          # Special handling for 12 Hour Snow - also sparse
          cat("12 Hour Snow line:", line, "\n")
          all_data[[field_names[i]]] <- parse_sparse_line(line)
          cat("Parsed 12 Hour Snow values:", all_data[[field_names[i]]], "\n")
        } else {
          is_numeric <- !(field_names[i] %in% c("Cloud Cover", "Wind Dir", "Precip Type"))
          all_data[[field_names[i]]] <- parse_fixed_width_line(line, is_numeric)
        }
      } else {
        all_data[[field_names[i]]] <- rep(NA, 9)
      }
    }
    
    # Create a dataframe in table format with all rows and columns
    data <- data.frame(
      Field = c("Time", field_names),
      stringsAsFactors = FALSE
    )
    
    # Add columns for each time period (all 9)
    for(i in 1:9) {
      col_name <- time_labels[i]
      # Use original time label for display in Time row
      display_time <- if(i <= length(time_labels_raw)) time_labels_raw[i] else ""
      col_values <- c(display_time)  # First value is the display time label
      
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
    cat("Column names:", names(data), "\n")
    cat("Number of rows:", nrow(data), "\n")
    cat("First few values of column 2:", data[[2]][1:3], "\n")
    cat("First few values of last column:", data[[ncol(data)]][1:3], "\n")
    
    # Store parsed numeric data for calculations (only first 5 for 24-hour period)
    attr(data, "temps") <- all_data[["Temperature"]][1:5]
    attr(data, "wind_dirs") <- all_data[["Wind Dir"]][1:5]
    attr(data, "wind_speeds") <- all_data[["Wind (mph)"]][1:5]
    attr(data, "wind_gusts") <- all_data[["Wind Gust (mph)"]][1:5]
    attr(data, "min_max") <- all_data[["Min/Max Temp"]]
    
    cat("Parsed forecast data successfully\n")
    cat("24-hour Temperature values (first 5 cols):", all_data[["Temperature"]][1:5], "\n")
    cat("24-hour Wind directions (first 5 cols):", all_data[["Wind Dir"]][1:5], "\n")
    cat("24-hour Wind speeds (first 5 cols):", all_data[["Wind (mph)"]][1:5], "\n")
    cat("24-hour Wind gusts (first 5 cols):", all_data[["Wind Gust (mph)"]][1:5], "\n")
    
    return(data)
    
  }, error = function(e) {
    message("Error fetching NWS data: ", e$message)
    return(NULL)
  })
}

# Function to calculate form values from 24-hour forecast
calculate_form_values <- function(data) {
  if(is.null(data) || nrow(data) == 0) {
    return(list(
      wind_avg_SB = 0,
      wind_max_SB = 0,
      dir_avg_SB = 0,
      temp_min_SB = 32,
      temp_max_SB = 32
    ))
  }
  
  # Extract the numeric data from attributes
  temps <- attr(data, "temps")
  wind_dirs <- attr(data, "wind_dirs")
  wind_speeds <- attr(data, "wind_speeds")
  wind_gusts <- attr(data, "wind_gusts")
  min_max <- attr(data, "min_max")
  
  # Calculate daily averages across the 24-hour period (5 time periods)
  # Average wind speed
  wind_avg <- mean(wind_speeds, na.rm = TRUE)
  if(is.na(wind_avg) || is.infinite(wind_avg)) wind_avg <- 0
  
  # Maximum wind gust - keep as 0 if no gust data
  wind_max <- 0
  if(any(!is.na(wind_gusts))) {
    wind_max <- max(wind_gusts, na.rm = TRUE)
    if(is.na(wind_max) || is.infinite(wind_max)) wind_max <- 0
  }
  
  # Average wind direction (most common direction)
  valid_dirs <- wind_dirs[!is.na(wind_dirs)]
  if(length(valid_dirs) > 0) {
    # Get mode (most frequent direction)
    dir_string <- names(sort(table(valid_dirs), decreasing = TRUE))[1]
  } else {
    dir_string <- "N"
  }
  
  # Temperature min/max
  # Use Min/Max values if available, otherwise calculate from temps
  temp_min_candidates <- c(min_max[!is.na(min_max)], 
                           min(temps, na.rm = TRUE))
  temp_max_candidates <- c(min_max[!is.na(min_max)], 
                           max(temps, na.rm = TRUE))
  
  temp_min <- min(temp_min_candidates, na.rm = TRUE)
  temp_max <- max(temp_max_candidates, na.rm = TRUE)
  
  # If still invalid, use defaults
  if(is.na(temp_min) || is.infinite(temp_min)) temp_min <- 32
  if(is.na(temp_max) || is.infinite(temp_max)) temp_max <- 32
  
  list(
    wind_avg_SB = round(wind_avg, 1),
    wind_max_SB = round(wind_max, 1),
    dir_avg_SB = dir_string,
    temp_min_SB = round(temp_min, 1),
    temp_max_SB = round(temp_max, 1)
  )
}

# UI
ui <- fluidPage(
  titlePanel("NWS Weather Data Form - Turnagain Pass Upper Elevations"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("fetch_data", "Fetch Latest Forecast", 
                   class = "btn-primary", 
                   style = "width: 100%; margin-bottom: 20px;"),
      actionButton("reset_form", "Reset Form", 
                   class = "btn-warning",
                   style = "width: 100%; margin-bottom: 20px;"),
      hr(),
      h4("Data Status:"),
      textOutput("data_status"),
      hr(),
      h4("Instructions:"),
      p("1. Click 'Fetch Latest Forecast' to populate fields"),
      p("2. Edit any values as needed"),
      p("3. Data from NWS Anchorage Avalanche Weather Guidance")
    ),
    
    mainPanel(
      h3("Editable Form Fields"),
      
      h4("Turnagain Pass Upper Elevations (above 3000 ft)"),
      fluidRow(
        column(6,
               numericInput("wind_avg_SB", 
                           "Daily Average Wind Speed (mph):", 
                           value = 0, step = 0.1),
               numericInput("wind_max_SB", 
                           "Daily Max Wind Gust (mph):", 
                           value = 0, step = 0.1),
               selectInput("dir_avg_SB", 
                           "Average Wind Direction:", 
                           choices = c("N", "NNE", "NE", "ENE", "E", "ESE", 
                                     "SE", "SSE", "S", "SSW", "SW", "WSW", 
                                     "W", "WNW", "NW", "NNW"),
                           selected = "N")
        ),
        column(6,
               numericInput("temp_min_SB", 
                           "Daily Min Temperature (°F):", 
                           value = 32, step = 0.1),
               numericInput("temp_max_SB", 
                           "Daily Max Temperature (°F):", 
                           value = 32, step = 0.1)
        )
      ),
      
      hr(),
      
      h4("Turnagain Pass Mid Elevations (1500 to 3000 ft)"),
      fluidRow(
        column(6,
               numericInput("wind_avg_mid", 
                           "Daily Average Wind Speed (mph):", 
                           value = 0, step = 0.1),
               numericInput("wind_max_mid", 
                           "Daily Max Wind Gust (mph):", 
                           value = 0, step = 0.1),
               selectInput("dir_avg_mid", 
                           "Average Wind Direction:", 
                           choices = c("N", "NNE", "NE", "ENE", "E", "ESE", 
                                     "SE", "SSE", "S", "SSW", "SW", "WSW", 
                                     "W", "WNW", "NW", "NNW"),
                           selected = "N")
        ),
        column(6,
               numericInput("temp_min_mid", 
                           "Daily Min Temperature (°F):", 
                           value = 32, step = 0.1),
               numericInput("temp_max_mid", 
                           "Daily Max Temperature (°F):", 
                           value = 32, step = 0.1)
        )
      ),
      
      hr(),
      
      h4("Export Form Data"),
      downloadButton("download_data", "Download as CSV"),
      
      hr(),
      
      h4("Turnagain Pass Upper Elevations (above 3000 ft)"),
      p("(Form fields use first 5 columns - 24-hour period)"),
      div(style = "overflow-x: auto;",
          tableOutput("raw_data_table")
      ),
      
      hr(),
      
      h4("Turnagain Pass Mid Elevations (1500 to 3000 ft)"),
      p("(Form fields use first 5 columns - 24-hour period)"),
      div(style = "overflow-x: auto;",
          tableOutput("mid_data_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store fetched data
  nws_data_upper <- reactiveVal(NULL)
  nws_data_mid <- reactiveVal(NULL)
  
  # Fetch data when button is clicked
  observeEvent(input$fetch_data, {
    # Show progress
    showModal(modalDialog(
      title = "Fetching Data",
      "Please wait while we retrieve the latest NWS forecast...",
      footer = NULL
    ))
    
    # Fetch Upper Elevations
    data_upper <- fetch_nws_data("Turnagain Pass Upper Elevations")
    nws_data_upper(data_upper)
    
    # Fetch Mid Elevations
    data_mid <- fetch_nws_data("Turnagain Pass Mid Elevations")
    nws_data_mid(data_mid)
    
    if(!is.null(data_upper)) {
      # Calculate form values for Upper
      form_values_upper <- calculate_form_values(data_upper)
      
      # Update Upper fields
      updateNumericInput(session, "wind_avg_SB", value = form_values_upper$wind_avg_SB)
      updateNumericInput(session, "wind_max_SB", value = form_values_upper$wind_max_SB)
      updateSelectInput(session, "dir_avg_SB", selected = form_values_upper$dir_avg_SB)
      updateNumericInput(session, "temp_min_SB", value = form_values_upper$temp_min_SB)
      updateNumericInput(session, "temp_max_SB", value = form_values_upper$temp_max_SB)
    }
    
    if(!is.null(data_mid)) {
      # Calculate form values for Mid
      form_values_mid <- calculate_form_values(data_mid)
      
      # Update Mid fields
      updateNumericInput(session, "wind_avg_mid", value = form_values_mid$wind_avg_SB)
      updateNumericInput(session, "wind_max_mid", value = form_values_mid$wind_max_SB)
      updateSelectInput(session, "dir_avg_mid", selected = form_values_mid$dir_avg_SB)
      updateNumericInput(session, "temp_min_mid", value = form_values_mid$temp_min_SB)
      updateNumericInput(session, "temp_max_mid", value = form_values_mid$temp_max_SB)
    }
    
    removeModal()
  })
  
  # Reset form
  observeEvent(input$reset_form, {
    # Reset Upper fields
    updateNumericInput(session, "wind_avg_SB", value = 0)
    updateNumericInput(session, "wind_max_SB", value = 0)
    updateSelectInput(session, "dir_avg_SB", selected = "N")
    updateNumericInput(session, "temp_min_SB", value = 32)
    updateNumericInput(session, "temp_max_SB", value = 32)
    
    # Reset Mid fields
    updateNumericInput(session, "wind_avg_mid", value = 0)
    updateNumericInput(session, "wind_max_mid", value = 0)
    updateSelectInput(session, "dir_avg_mid", selected = "N")
    updateNumericInput(session, "temp_min_mid", value = 32)
    updateNumericInput(session, "temp_max_mid", value = 32)
  })
  
  # Data status
  output$data_status <- renderText({
    data_upper <- nws_data_upper()
    data_mid <- nws_data_mid()
    if(is.null(data_upper) && is.null(data_mid)) {
      "No data loaded. Click 'Fetch Latest Forecast' to begin."
    } else {
      msg <- ""
      if(!is.null(data_upper)) msg <- paste0(msg, "Upper Elevations loaded. ")
      if(!is.null(data_mid)) msg <- paste0(msg, "Mid Elevations loaded. ")
      paste0(msg, "Data from NWS Anchorage.")
    }
  })
  
  # Raw data tables
  output$raw_data_table <- renderTable({
    data <- nws_data_upper()
    if(is.null(data)) {
      return(data.frame(Message = "No data loaded yet"))
    }
    data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  output$mid_data_table <- renderTable({
    data <- nws_data_mid()
    if(is.null(data)) {
      return(data.frame(Message = "No data loaded yet"))
    }
    data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("nws_weather_form_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Convert wind direction strings to degrees
      dir_map <- c(N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5, 
                   SE=135, SSE=157.5, S=180, SSW=202.5, SW=225, WSW=247.5,
                   W=270, WNW=292.5, NW=315, NNW=337.5)
      
      dir_upper_deg <- ifelse(input$dir_avg_SB %in% names(dir_map), 
                              dir_map[input$dir_avg_SB], 0)
      dir_mid_deg <- ifelse(input$dir_avg_mid %in% names(dir_map), 
                            dir_map[input$dir_avg_mid], 0)
      
      form_data <- data.frame(
        Field = c("wind_avg_hi", "wind_max_hi", "dir_avg_hi", 
                 "temp_min_hi", "temp_max_hi",
                 "wind_avg_mid", "wind_max_mid", "dir_avg_mid",
                 "temp_min_mid", "temp_max_mid"),
        Description = c("Daily average wind speed at Turnagain Pass Upper Elevations (mph)",
                       "Daily max wind gust at Turnagain Pass Upper Elevations (mph)",
                       "Average wind direction at Turnagain Pass Upper Elevations (deg)",
                       "Daily min temperature at Turnagain Pass Upper Elevations (F)",
                       "Daily max temp Turnagain Pass Upper Elevations (F)",
                       "Daily average wind speed at Turnagain Pass Mid Elevations (mph)",
                       "Daily max wind gust at Turnagain Pass Mid Elevations (mph)",
                       "Average wind direction at Turnagain Pass Mid Elevations (deg)",
                       "Daily min temperature at Turnagain Pass Mid Elevations (F)",
                       "Daily max temp Turnagain Pass Mid Elevations (F)"),
        Value = c(input$wind_avg_SB, input$wind_max_SB, dir_upper_deg,
                 input$temp_min_SB, input$temp_max_SB,
                 input$wind_avg_mid, input$wind_max_mid, dir_mid_deg,
                 input$temp_min_mid, input$temp_max_mid)
      )
      write.csv(form_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
