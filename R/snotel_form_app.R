library(shiny)
library(dplyr)
library(lubridate)

# Function to fetch and process SNOTEL data
fetch_snotel_data <- function() {
  url <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/start_of_period/954:AK:SNTL%7Cid=%22%22%7Cname/CurrentWY,CurrentWYEnd/WTEQ::value,SNWD::value,PREC::value,TMAX::value,TMIN::value?fitToScreen=false"
  
  tryCatch({
    # Read all lines first to find the header
    all_lines <- readLines(url, warn = FALSE)
    
    # Find the line that contains "Date" - this is the header row
    header_line <- grep("^Date,", all_lines)[1]
    
    if(is.na(header_line)) {
      stop("Could not find header row in CSV")
    }
    
    cat(paste0("Found header at line ", header_line, "\n"))
    
    # Read the CSV starting from the header line
    data <- read.csv(url, skip = header_line - 1, header = TRUE, 
                     stringsAsFactors = FALSE, check.names = FALSE)
    
    # Clean up column names - remove any extra characters
    names(data) <- trimws(names(data))
    names(data) <- gsub("\\s+", "_", names(data))
    
    # Convert date column
    date_col <- grep("Date", names(data), ignore.case = TRUE)[1]
    if(!is.na(date_col)) {
      data[[date_col]] <- as.Date(data[[date_col]], format = "%Y-%m-%d")
      names(data)[date_col] <- "Date"
    }
    
    # Remove any rows with NA dates
    data <- data[!is.na(data$Date), ]
    
    # Sort by date descending to get most recent first
    data <- data %>% arrange(desc(Date))
    
    return(data)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)
  })
}

# Function to calculate cumulative seasonal values (from Oct 1)
calculate_seasonal_cumulative <- function(data, value_col) {
  if(nrow(data) == 0) return(NA)
  
  # Get current water year start (Oct 1)
  current_date <- data$Date[1]
  year <- year(current_date)
  water_year_start <- if(month(current_date) >= 10) {
    as.Date(paste0(year, "-10-01"))
  } else {
    as.Date(paste0(year - 1, "-10-01"))
  }
  
  # Filter data from water year start to current
  seasonal_data <- data %>% 
    filter(Date >= water_year_start, Date <= current_date) %>%
    arrange(Date)
  
  # Sum non-NA values
  sum(seasonal_data[[value_col]], na.rm = TRUE)
}

# Function to calculate multi-day totals (using incremental changes for cumulative data)
calculate_nday_total <- function(data, value_col, n_days) {
  if(nrow(data) < n_days + 1) return(NA)
  
  # Calculate the difference between current value and value n_days ago
  # This gives us the net change over the period
  current_value <- ifelse(is.na(data[[value_col]][1]), 0, data[[value_col]][1])
  past_value <- ifelse(is.na(data[[value_col]][n_days + 1]), 0, data[[value_col]][n_days + 1])
  
  # Return the positive change (new accumulation)
  max(0, current_value - past_value)
}

# Function to calculate form values from data
calculate_form_values <- function(data) {
  if(is.null(data) || nrow(data) == 0) {
    return(list(
      snow_depth = 0,
      swe_in = 0,
      swe_increment_in = 0,
      precip_increment_in = 0,
      precip_cumulative_in = 0,
      temp_max_CR = 0,
      temp_min_CR = 0,
      snow_depth_3day = 0,
      swe_increment_3day = 0,
      precip_increment_3day = 0,
      snow_depth_7day = 0,
      swe_increment_7day = 0,
      precip_increment_7day = 0,
      snow_depth_increment = 0
    ))
  }
  
  # Find column names (they may vary slightly)
  # Look for columns with these patterns
  all_cols <- names(data)
  
  # Find SWE column (Snow Water Equivalent)
  swe_col <- all_cols[grep("WTEQ|Snow.*Water|SWE", all_cols, ignore.case = TRUE)][1]
  
  # Find Snow Depth column
  snow_depth_col <- all_cols[grep("SNWD|Snow.*Depth", all_cols, ignore.case = TRUE)][1]
  
  # Find Precipitation column
  precip_col <- all_cols[grep("PREC|Precipitation|Precip\\.", all_cols, ignore.case = TRUE)][1]
  
  # Find Temperature columns (Max and Min) - be more flexible with patterns
  temp_max_col <- all_cols[grep("TMAX|Max.*Temp|Temperature.*Max", all_cols, ignore.case = TRUE)][1]
  temp_min_col <- all_cols[grep("TMIN|Min.*Temp|Temperature.*Min", all_cols, ignore.case = TRUE)][1]
  
  # Check if we found all necessary columns
  if(is.na(swe_col) || is.na(snow_depth_col) || is.na(precip_col)) {
    cat("Warning: Could not find all required columns\n")
    cat("Available columns:", paste(all_cols, collapse = ", "), "\n")
  }
  
  # Current values (most recent day)
  current_snow_depth <- ifelse(is.na(data[[snow_depth_col]][1]), 0, data[[snow_depth_col]][1])
  current_swe <- ifelse(is.na(data[[swe_col]][1]), 0, data[[swe_col]][1])
  
  # 24-hour increments (calculate change from previous day for all cumulative values)
  swe_24h <- if(nrow(data) >= 2) {
    max(0, data[[swe_col]][1] - data[[swe_col]][2], na.rm = TRUE)
  } else { 0 }
  
  # Precipitation increment (change from yesterday)
  precip_24h <- if(nrow(data) >= 2) {
    max(0, data[[precip_col]][1] - data[[precip_col]][2], na.rm = TRUE)
  } else { 0 }
  
  snow_depth_24h <- if(nrow(data) >= 2) {
    max(0, data[[snow_depth_col]][1] - data[[snow_depth_col]][2], na.rm = TRUE)
  } else { 0 }
  
  # Seasonal cumulative values
  # All SNOTEL values appear to be cumulative for the water year
  swe_cumulative <- ifelse(is.na(current_swe), 0, current_swe)
  
  # Precipitation is also cumulative in SNOTEL
  precip_cumulative <- ifelse(is.na(data[[precip_col]][1]), 0, data[[precip_col]][1])
  
  # Temperature min/max (from most recent day)
  temp_max <- if(!is.na(temp_max_col)) {
    ifelse(is.na(data[[temp_max_col]][1]), 32, data[[temp_max_col]][1])
  } else { 32 }
  
  temp_min <- if(!is.na(temp_min_col)) {
    ifelse(is.na(data[[temp_min_col]][1]), 32, data[[temp_min_col]][1])
  } else { 32 }
  
  # Multi-day totals
  # All values are cumulative, so calculate net change over the period
  snow_3day <- calculate_nday_total(data, snow_depth_col, 3)
  swe_3day <- calculate_nday_total(data, swe_col, 3)
  precip_3day <- calculate_nday_total(data, precip_col, 3)
  
  snow_7day <- calculate_nday_total(data, snow_depth_col, 7)
  swe_7day <- calculate_nday_total(data, swe_col, 7)
  precip_7day <- calculate_nday_total(data, precip_col, 7)
  
  list(
    snow_depth = round(current_snow_depth, 2),
    swe_in = round(current_swe, 2),
    swe_increment_in = round(swe_24h, 2),
    precip_increment_in = round(precip_24h, 2),
    precip_cumulative_in = round(precip_cumulative, 2),
    temp_max_CR = round(temp_max, 1),
    temp_min_CR = round(temp_min, 1),
    snow_depth_3day = round(snow_3day, 2),
    swe_increment_3day = round(swe_3day, 2),
    precip_increment_3day = round(precip_3day, 2),
    snow_depth_7day = round(snow_7day, 2),
    swe_increment_7day = round(swe_7day, 2),
    precip_increment_7day = round(precip_7day, 2),
    snow_depth_increment = round(snow_depth_24h, 2)
  )
}

# UI
ui <- fluidPage(
  titlePanel("SNOTEL Data Form - Station 954 (Alaska) - Current Water Year"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("fetch_data", "Fetch Latest Data", 
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
      p("1. Click 'Fetch Latest Data' to populate fields"),
      p("2. Edit any values as needed"),
      p("3. Data fetched from current water year (Oct 1 - present)")
    ),
    
    mainPanel(
      h3("Editable Form Fields"),
      
      fluidRow(
        column(6,
               numericInput("snow_depth", 
                           "Cumulative Seasonal Snow Depth (in):", 
                           value = 0, step = 0.1),
               numericInput("swe_in", 
                           "Cumulative Seasonal SWE (in):", 
                           value = 0, step = 0.1),
               numericInput("swe_increment_in", 
                           "24-hour Incremental SWE (in):", 
                           value = 0, step = 0.1),
               numericInput("precip_increment_in", 
                           "24-h Incremental Precip (in):", 
                           value = 0, step = 0.1),
               numericInput("precip_cumulative_in", 
                           "Seasonal Cumulative Precip (in):", 
                           value = 0, step = 0.1),
               numericInput("temp_max_CR", 
                           "Max Daily Temp at Center Ridge (°F):", 
                           value = 32, step = 0.1),
               numericInput("temp_min_CR", 
                           "Min Daily Temp at Center Ridge (°F):", 
                           value = 32, step = 0.1)
        ),
        column(6,
               numericInput("snow_depth_3day", 
                           "3-day Snow Total (in):", 
                           value = 0, step = 0.1),
               numericInput("swe_increment_3day", 
                           "3-day SWE Total (in):", 
                           value = 0, step = 0.1),
               numericInput("precip_increment_3day", 
                           "3-day Precip Total (in):", 
                           value = 0, step = 0.1),
               numericInput("snow_depth_7day", 
                           "7-day Snow Total (in):", 
                           value = 0, step = 0.1),
               numericInput("swe_increment_7day", 
                           "7-day SWE Total (in):", 
                           value = 0, step = 0.1),
               numericInput("precip_increment_7day", 
                           "7-day Precip Total (in):", 
                           value = 0, step = 0.1),
               numericInput("snow_depth_increment", 
                           "24-h Snow Total (in):", 
                           value = 0, step = 0.1)
        )
      ),
      
      hr(),
      
      h4("Export Form Data"),
      downloadButton("download_data", "Download as CSV"),
      
      hr(),
      
      h4("Raw Data Preview (Last 7 Days)"),
      tableOutput("raw_data_table"),
      
      hr(),
      
      h4("Data Visualizations"),
      fluidRow(
        column(6, plotOutput("snow_depth_plot", height = "300px")),
        column(6, plotOutput("swe_plot", height = "300px"))
      ),
      fluidRow(
        column(6, plotOutput("precip_plot", height = "300px")),
        column(6, plotOutput("temp_plot", height = "300px"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store fetched data
  snotel_data <- reactiveVal(NULL)
  
  # Fetch data when button is clicked
  observeEvent(input$fetch_data, {
    # Show progress
    showModal(modalDialog(
      title = "Fetching Data",
      "Please wait while we retrieve the latest SNOTEL data...",
      footer = NULL
    ))
    
    data <- fetch_snotel_data()
    snotel_data(data)
    
    if(!is.null(data)) {
      # Calculate form values
      form_values <- calculate_form_values(data)
      
      # Update all form fields
      updateNumericInput(session, "snow_depth", value = form_values$snow_depth)
      updateNumericInput(session, "swe_in", value = form_values$swe_in)
      updateNumericInput(session, "swe_increment_in", value = form_values$swe_increment_in)
      updateNumericInput(session, "precip_increment_in", value = form_values$precip_increment_in)
      updateNumericInput(session, "precip_cumulative_in", value = form_values$precip_cumulative_in)
      updateNumericInput(session, "temp_max_CR", value = form_values$temp_max_CR)
      updateNumericInput(session, "temp_min_CR", value = form_values$temp_min_CR)
      updateNumericInput(session, "snow_depth_3day", value = form_values$snow_depth_3day)
      updateNumericInput(session, "swe_increment_3day", value = form_values$swe_increment_3day)
      updateNumericInput(session, "precip_increment_3day", value = form_values$precip_increment_3day)
      updateNumericInput(session, "snow_depth_7day", value = form_values$snow_depth_7day)
      updateNumericInput(session, "swe_increment_7day", value = form_values$swe_increment_7day)
      updateNumericInput(session, "precip_increment_7day", value = form_values$precip_increment_7day)
      updateNumericInput(session, "snow_depth_increment", value = form_values$snow_depth_increment)
    }
    
    removeModal()
  })
  
  # Reset form
  observeEvent(input$reset_form, {
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
  })
  
  # Data status
  output$data_status <- renderText({
    data <- snotel_data()
    if(is.null(data)) {
      "No data loaded. Click 'Fetch Latest Data' to begin."
    } else {
      paste0("Data loaded successfully! ", nrow(data), " days of data available. ",
             "Most recent date: ", format(data$Date[1], "%m/%d/%Y"))
    }
  })
  
  # Raw data table
  output$raw_data_table <- renderTable({
    data <- snotel_data()
    if(is.null(data)) {
      return(data.frame(Message = "No data loaded yet"))
    }
    # Format the date column as mm/dd/yyyy for display
    display_data <- head(data, 7)
    if("Date" %in% names(display_data)) {
      display_data$Date <- format(display_data$Date, "%m/%d/%Y")
    }
    display_data
  })
  
  # Snow Depth Plot
  output$snow_depth_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    
    # Get column name
    snow_col <- grep("SNWD|Snow.*Depth", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(snow_col)) return(NULL)
    
    # Plot last 30 days
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[snow_col]], 
         type = "l", lwd = 2, col = "blue",
         xlab = "Date", ylab = "Snow Depth (inches)",
         main = "Snow Depth - Last 30 Days")
    grid()
  })
  
  # SWE Plot
  output$swe_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    
    # Get column name
    swe_col <- grep("WTEQ|Snow.*Water|SWE", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(swe_col)) return(NULL)
    
    # Plot last 30 days
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[swe_col]], 
         type = "l", lwd = 2, col = "darkblue",
         xlab = "Date", ylab = "SWE (inches)",
         main = "Snow Water Equivalent - Last 30 Days")
    grid()
  })
  
  # Precipitation Plot
  output$precip_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    
    # Get column name
    precip_col <- grep("PREC|Precipitation|Precip\\.", names(data), value = TRUE, ignore.case = TRUE)[1]
    if(is.na(precip_col)) return(NULL)
    
    # Plot last 30 days
    plot_data <- head(data, 30)
    plot(plot_data$Date, plot_data[[precip_col]], 
         type = "l", lwd = 2, col = "green4",
         xlab = "Date", ylab = "Cumulative Precipitation (inches)",
         main = "Precipitation - Last 30 Days")
    grid()
  })
  
  # Temperature Plot
  output$temp_plot <- renderPlot({
    data <- snotel_data()
    if(is.null(data)) return(NULL)
    
    # Get column names
    temp_max_col <- grep("TMAX|Max.*Temp|Temperature.*Max", names(data), value = TRUE, ignore.case = TRUE)[1]
    temp_min_col <- grep("TMIN|Min.*Temp|Temperature.*Min", names(data), value = TRUE, ignore.case = TRUE)[1]
    
    if(is.na(temp_max_col) && is.na(temp_min_col)) return(NULL)
    
    # Plot last 30 days
    plot_data <- head(data, 30)
    
    # Set up plot range
    y_range <- range(c(plot_data[[temp_max_col]], plot_data[[temp_min_col]]), na.rm = TRUE)
    
    plot(plot_data$Date, plot_data[[temp_max_col]], 
         type = "l", lwd = 2, col = "red",
         xlab = "Date", ylab = "Temperature (°F)",
         main = "Temperature - Last 30 Days",
         ylim = y_range)
    lines(plot_data$Date, plot_data[[temp_min_col]], lwd = 2, col = "blue")
    legend("topright", legend = c("Max Temp", "Min Temp"), 
           col = c("red", "blue"), lwd = 2, bty = "n")
    grid()
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("snotel_form_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      form_data <- data.frame(
        Field = c("snow_depth", "swe_in", "swe_increment_in", "precip_increment_in",
                 "precip_cumulative_in", "temp_max_CR", "temp_min_CR", "snow_depth_3day",
                 "swe_increment_3day", "precip_increment_3day", "snow_depth_7day",
                 "swe_increment_7day", "precip_increment_7day", "snow_depth_increment"),
        Description = c("Cumulative seasonal snow depth (in)", 
                       "Cumulative seasonal SWE (in)",
                       "24-hour incremental SWE (in)", 
                       "24-h incremental precip (in)",
                       "Seasonal cumulative precip (in)", 
                       "Max daily temp at Center Ridge (F)",
                       "Min daily temp at Center Ridge (F)", 
                       "3-day snow total (in)",
                       "3-day SWE total (in)", 
                       "3-day precip total (in)",
                       "7-day snow total (in)", 
                       "7-day SWE total (in)",
                       "7-day precip total (in)", 
                       "24-h snow total (in)"),
        Value = c(input$snow_depth, input$swe_in, input$swe_increment_in,
                 input$precip_increment_in, input$precip_cumulative_in,
                 input$temp_max_CR, input$temp_min_CR, input$snow_depth_3day,
                 input$swe_increment_3day, input$precip_increment_3day,
                 input$snow_depth_7day, input$swe_increment_7day,
                 input$precip_increment_7day, input$snow_depth_increment)
      )
      write.csv(form_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
