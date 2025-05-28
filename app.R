library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(tidyr)
library(stringr)

ui <- dashboardPage(
  dashboardHeader(title = "UI Parking Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Peak Usage Analysis", tabName = "peak", icon = icon("chart-line")),
      menuItem("Access Group Patterns", tabName = "groups", icon = icon("users")),
      menuItem("Lot Utilization", tabName = "lots", icon = icon("car")),
      #menuItem("Forecasting", tabName = "forecast", icon = icon("chart-area")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_transactions"),
                valueBoxOutput("unique_cards"),
                valueBoxOutput("active_lots")
              ),
              
              fluidRow(
                box(
                  title = "Transaction Volume Over Time", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("transaction_timeline", height = "400px")
                ),
                
                box(
                  title = "Top 10 Most Used Lots", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("top_lots", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hourly Usage Patterns", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("hourly_patterns", height = "350px")
                ),
                
                box(
                  title = "Weekly Usage Distribution", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("weekly_patterns", height = "350px")
                )
              )
      ),
      
      # Peak Usage Analysis Tab
      tabItem(tabName = "peak",
              fluidRow(
                box(
                  title = "Peak Analysis Controls", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("peak_lot", "Select Parking Lot:",
                              choices = NULL, 
                              selected = NULL),
                  
                  dateRangeInput("peak_date_range", "Date Range:",
                                 start = "2024-01-01",
                                 end = "2025-04-30"),
                  
                  radioButtons("peak_period", "Analysis Period:",
                               choices = list("Daily" = "day",
                                              "Weekly" = "week", 
                                              "Monthly" = "month"),
                               selected = "day"),
                  
                  actionButton("analyze_peak", "Analyze Peak Usage", 
                               class = "btn-primary btn-block")
                ),
                
                box(
                  title = "Peak Usage Visualization", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("peak_usage_plot", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Peak Usage Summary", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("peak_summary_table")
                )
              )
      ),
      
      # Access Group Patterns Tab
      tabItem(tabName = "groups",
              fluidRow(
                box(
                  title = "Access Group Usage Distribution", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("group_distribution", height = "400px")
                ),
                
                box(
                  title = "Group Usage by Time of Day", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("group_hourly", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Monthly Access Group Trends", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("group_monthly_trends", height = "400px")
                )
              )
      ),
      
      # Lot Utilization Tab
      tabItem(tabName = "lots",
              fluidRow(
                box(
                  title = "Lot Comparison Controls", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("compare_lots", "Select Lots to Compare:",
                              choices = NULL,
                              multiple = TRUE),
                  
                  dateRangeInput("lot_date_range", "Date Range:",
                                 start = "2024-01-01",
                                 end = "2025-04-30"),
                  
                  actionButton("compare_lots_btn", "Compare Lots", 
                               class = "btn-success btn-block")
                ),
                
                box(
                  title = "Lot Usage Comparison", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("lot_comparison", height = "450px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Entry vs Exit Patterns", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("entry_exit_patterns", height = "350px")
                ),
                
                box(
                  title = "Overnight Parking Analysis", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overnight_analysis", height = "350px")
                )
              )
      ),
      
      
      
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Data Summary", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("data_summary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Filters", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 3,
                  dateRangeInput("data_date_range", "Date Range:",
                                 start = "2024-01-01",
                                 end = "2025-04-30"),
                  
                  selectInput("data_lots", "Parking Lots:",
                              choices = NULL,
                              multiple = TRUE),
                  
                  downloadButton("download_filtered", "Download Filtered Data",
                                 class = "btn-info btn-block")
                ),
                
                box(
                  title = "Transaction Data Sample", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 9,
                  DT::dataTableOutput("transaction_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Data loading - using the exact same logic that worked
  parking_data <- reactive({
    file_path <- "/Users/deepikamitta/Downloads/Data Practice/CardTransaction.csv"
    
    tryCatch({
      raw_data <- read.csv(file_path, stringsAsFactors = FALSE)
      message("Loaded ", nrow(raw_data), " rows from parking data")
      
      # Process exactly like the working version
      processed <- raw_data
      processed$EntranceTime <- ifelse(processed$EntranceTime == "NULL" | processed$EntranceTime == "", NA, processed$EntranceTime)
      processed$ExitTime <- ifelse(processed$ExitTime == "NULL" | processed$ExitTime == "", NA, processed$ExitTime)
      
      # Determine date format
      test_time <- processed$EntranceTime[!is.na(processed$EntranceTime)][1]
      if(!is.na(test_time)) {
        test_parse <- as.POSIXct(test_time, format = "%Y-%m-%d %H:%M:%S")
        date_format <- if(is.na(test_parse)) "%m/%d/%Y %H:%M:%S" else "%Y-%m-%d %H:%M:%S"
        
        processed$EntranceTime_parsed <- as.POSIXct(processed$EntranceTime, format = date_format)
        processed$ExitTime_parsed <- as.POSIXct(processed$ExitTime, format = date_format)
        processed$Date <- as.Date(coalesce(processed$EntranceTime_parsed, processed$ExitTime_parsed))
        processed$Hour <- hour(coalesce(processed$EntranceTime_parsed, processed$ExitTime_parsed))
        processed$Weekday <- weekdays(processed$Date)
        processed$Month <- month(processed$Date)
        processed$Year <- year(processed$Date)
        
        final_data <- processed[!is.na(processed$Date), ]
        message("Final processed data: ", nrow(final_data), " valid rows")
        return(final_data)
      }
      
      return(data.frame())
      
    }, error = function(e) {
      message("Error loading data: ", e$message)
      return(data.frame())
    })
  })
  
  # Update UI choices
  observe({
    data <- parking_data()
    
    if(nrow(data) > 0) {
      lot_choices <- sort(unique(data$LotNumber))
      
      updateSelectInput(session, "peak_lot", choices = lot_choices, selected = lot_choices[1])
      updateSelectInput(session, "forecast_lot", choices = lot_choices, selected = lot_choices[1])
      updateSelectInput(session, "compare_lots", choices = lot_choices)
      updateSelectInput(session, "data_lots", choices = lot_choices)
    }
  })
  
  # Overview Tab Outputs
  output$total_transactions <- renderValueBox({
    data <- parking_data()
    valueBox(
      value = formatC(nrow(data), format = "d", big.mark = ","),
      subtitle = "Total Transactions",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$unique_cards <- renderValueBox({
    data <- parking_data()
    count <- if(nrow(data) > 0) length(unique(data$CardNumber)) else 0
    valueBox(
      value = formatC(count, format = "d", big.mark = ","),
      subtitle = "Unique Cards",
      icon = icon("id-card"),
      color = "green"
    )
  })
  
  output$active_lots <- renderValueBox({
    data <- parking_data()
    count <- if(nrow(data) > 0) length(unique(data$LotNumber)) else 0
    valueBox(
      value = count,
      subtitle = "Active Parking Lots",
      icon = icon("car"),
      color = "yellow"
    )
  })
  
  output$transaction_timeline <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Loading..."), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    daily_summary <- data %>%
      group_by(Date) %>%
      summarise(Transactions = n(), .groups = 'drop')
    
    p <- ggplot(daily_summary, aes(x = Date, y = Transactions)) +
      geom_line(color = "#3498db", size = 0.8) +
      geom_smooth(method = "loess", se = FALSE, color = "#e74c3c", size = 1) +
      labs(title = "Daily Transaction Volume", x = "Date", y = "Transactions") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$top_lots <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Loading..."), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    top_lots <- data %>%
      group_by(LotNumber) %>%
      summarise(Transactions = n(), .groups = 'drop') %>%
      arrange(desc(Transactions)) %>%
      head(10)
    
    p <- ggplot(top_lots, aes(x = reorder(factor(LotNumber), Transactions), y = Transactions)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      coord_flip() +
      labs(title = "Most Active Lots", x = "Lot Number", y = "Transactions") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$hourly_patterns <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"Hour" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Loading..."), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    hourly_data <- data %>%
      filter(!is.na(Hour)) %>%
      group_by(Hour) %>%
      summarise(Count = n(), .groups = 'drop')
    
    p <- ggplot(hourly_data, aes(x = Hour, y = Count)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      labs(title = "Hourly Usage Patterns", x = "Hour of Day", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$weekly_patterns <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"Weekday" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Loading..."), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    weekly_data <- data %>%
      mutate(Weekday = factor(Weekday, levels = weekday_order)) %>%
      group_by(Weekday) %>%
      summarise(Transactions = n(), .groups = 'drop')
    
    p <- ggplot(weekly_data, aes(x = Weekday, y = Transactions)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      labs(title = "Usage by Day of Week", x = "Day of Week", y = "Transactions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Peak Analysis
  peak_data <- eventReactive(input$analyze_peak, {
    data <- parking_data()
    
    if(nrow(data) == 0) return(data.frame())
    
    filtered_data <- data %>%
      filter(
        LotNumber == input$peak_lot,
        Date >= input$peak_date_range[1],
        Date <= input$peak_date_range[2]
      )
    
    if (input$peak_period == "day") {
      peak_summary <- filtered_data %>%
        group_by(Date) %>%
        summarise(
          PeakUsage = n(),
          UniqueCards = n_distinct(CardNumber),
          .groups = 'drop'
        )
    } else if (input$peak_period == "week") {
      peak_summary <- filtered_data %>%
        mutate(Week = floor_date(Date, "week")) %>%
        group_by(Week) %>%
        summarise(
          PeakUsage = n(),
          UniqueCards = n_distinct(CardNumber),
          .groups = 'drop'
        )
    } else {
      peak_summary <- filtered_data %>%
        mutate(Month = floor_date(Date, "month")) %>%
        group_by(Month) %>%
        summarise(
          PeakUsage = n(),
          UniqueCards = n_distinct(CardNumber),
          .groups = 'drop'
        )
    }
    
    return(peak_summary)
  })
  
  output$peak_usage_plot <- renderPlotly({
    if(input$analyze_peak == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Click 'Analyze Peak Usage' to see results"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    data <- peak_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No data for selected parameters"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    time_col <- names(data)[1]
    
    p <- ggplot(data, aes_string(x = time_col)) +
      geom_line(aes(y = PeakUsage, color = "Usage Count"), size = 1.2) +
      geom_line(aes(y = UniqueCards, color = "Unique Cards"), size = 1) +
      scale_color_manual(values = c("Usage Count" = "#e74c3c", "Unique Cards" = "#3498db")) +
      labs(
        title = paste("Peak Usage Analysis -", str_to_title(input$peak_period)),
        x = str_to_title(input$peak_period),
        y = "Count",
        color = "Metric"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$peak_summary_table <- DT::renderDataTable({
    if(input$analyze_peak == 0) return(data.frame())
    
    data <- peak_data()
    if(nrow(data) == 0) return(data.frame())
    
    summary_data <- data %>%
      arrange(desc(PeakUsage)) %>%
      head(20)
    
    DT::datatable(summary_data, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Access Group Analysis
  output$group_distribution <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"EffectiveGroupNumber" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No access group data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    group_usage <- data %>%
      filter(EffectiveGroupNumber != -1) %>%
      group_by(EffectiveGroupNumber) %>%
      summarise(Usage = n(), .groups = 'drop') %>%
      arrange(desc(Usage)) %>%
      head(15)
    
    if(nrow(group_usage) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No valid access groups"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(group_usage, aes(x = reorder(factor(EffectiveGroupNumber), Usage), y = Usage)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      coord_flip() +
      labs(title = "Access Groups by Usage", x = "Access Group", y = "Usage") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$group_hourly <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"EffectiveGroupNumber" %in% names(data) || !"Hour" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No hourly group data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    group_hourly <- data %>%
      filter(EffectiveGroupNumber != -1, !is.na(Hour)) %>%
      group_by(Hour, EffectiveGroupNumber) %>%
      summarise(Usage = n(), .groups = 'drop') %>%
      group_by(EffectiveGroupNumber) %>%
      filter(sum(Usage) >= 100) %>%
      ungroup() %>%
      group_by(Hour) %>%
      slice_max(order_by = Usage, n = 3) %>%
      ungroup()
    
    if(nrow(group_hourly) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Insufficient data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(group_hourly, aes(x = Hour, y = Usage, color = factor(EffectiveGroupNumber))) +
      geom_line(size = 1) +
      labs(title = "Access Group Usage by Hour", x = "Hour", y = "Usage", color = "Group") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$group_monthly_trends <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"EffectiveGroupNumber" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No group trend data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    monthly_groups <- data %>%
      filter(EffectiveGroupNumber != -1, Date >= as.Date("2023-01-01")) %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth, EffectiveGroupNumber) %>%
      summarise(Usage = n(), .groups = 'drop') %>%
      group_by(EffectiveGroupNumber) %>%
      filter(sum(Usage) >= 1000) %>%
      ungroup()
    
    if(nrow(monthly_groups) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Insufficient data for trends"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(monthly_groups, aes(x = YearMonth, y = Usage, color = factor(EffectiveGroupNumber))) +
      geom_line(size = 1) +
      labs(title = "Monthly Access Group Trends", x = "Month", y = "Usage", color = "Access Group") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Lot Utilization
  lot_comparison_data <- eventReactive(input$compare_lots_btn, {
    data <- parking_data()
    
    if(nrow(data) == 0 || is.null(input$compare_lots)) return(data.frame())
    
    comparison_data <- data %>%
      filter(
        LotNumber %in% input$compare_lots,
        Date >= input$lot_date_range[1],
        Date <= input$lot_date_range[2]
      ) %>%
      group_by(Date, LotNumber) %>%
      summarise(DailyUsage = n(), .groups = 'drop')
    
    return(comparison_data)
  })
  
  output$lot_comparison <- renderPlotly({
    if(input$compare_lots_btn == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Select lots and click 'Compare Lots'"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    data <- lot_comparison_data()
    
    if(nrow(data) == 0) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No data for comparison"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(data, aes(x = Date, y = DailyUsage, color = factor(LotNumber))) +
      geom_line(size = 1) +
      labs(title = "Lot Usage Comparison", x = "Date", y = "Daily Usage", color = "Lot Number") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$entry_exit_patterns <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !all(c("NoEntry", "NoExit", "Hour") %in% names(data))) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No entry/exit data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    entry_exit_data <- data %>%
      filter(!is.na(Hour)) %>%
      group_by(Hour) %>%
      summarise(
        Entries = sum(NoEntry == 0, na.rm = TRUE),
        Exits = sum(NoExit == 0, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(Entries, Exits), names_to = "Type", values_to = "Count")
    
    p <- ggplot(entry_exit_data, aes(x = Hour, y = Count, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Entries" = "#2ecc71", "Exits" = "#e74c3c")) +
      labs(title = "Entry vs Exit Patterns", x = "Hour", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$overnight_analysis <- renderPlotly({
    data <- parking_data()
    
    if(nrow(data) == 0 || !"Overnight" %in% names(data)) {
      p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "No overnight data"), size = 5) + theme_void()
      return(ggplotly(p))
    }
    
    overnight_data <- data %>%
      filter(Date >= (max(Date, na.rm = TRUE) - days(90))) %>%
      group_by(Date) %>%
      summarise(
        OvernightCount = sum(Overnight == 1, na.rm = TRUE),
        TotalTransactions = n(),
        OvernightRate = OvernightCount / TotalTransactions * 100,
        .groups = 'drop'
      )
    
    p <- ggplot(overnight_data, aes(x = Date, y = OvernightRate)) +
      geom_line(color = "#8e44ad", size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "#e67e22") +
      labs(title = "Overnight Parking Rate (Last 90 Days)", x = "Date", y = "Overnight Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  
  
  # Data Explorer
  output$data_summary <- renderText({
    data <- parking_data()
    
    if(nrow(data) == 0) {
      return("No data loaded")
    }
    
    # Calculate key statistics
    date_range <- range(data$Date, na.rm = TRUE)
    missing_entrance <- sum(is.na(data$EntranceTime_parsed))
    missing_exit <- sum(is.na(data$ExitTime_parsed))
    
    # Top lots
    top_3_lots <- data %>%
      group_by(LotNumber) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count)) %>%
      head(3)
    
    # Peak hour
    peak_hour <- data %>%
      filter(!is.na(Hour)) %>%
      group_by(Hour) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count)) %>%
      slice(1)
    
    paste(
      "PARKING DATA SUMMARY\n",
      "===================\n",
      " Dataset Overview:\n",
      "• Total Records:", formatC(nrow(data), format = "d", big.mark = ","), "\n",
      "• Date Range:", date_range[1], "to", date_range[2], "\n",
      "• Duration:", as.numeric(date_range[2] - date_range[1]), "days\n\n",
      
      "Usage Statistics:\n",
      "• Unique Cards:", formatC(length(unique(data$CardNumber)), format = "d", big.mark = ","), "\n",
      "• Active Lots:", length(unique(data$LotNumber)), "\n",
      "• Average Daily Usage:", formatC(round(nrow(data) / as.numeric(date_range[2] - date_range[1])), format = "d", big.mark = ","), "\n\n",
      
      "Top Performers:\n",
      "• Busiest Lot:", top_3_lots$LotNumber[1], "(", formatC(top_3_lots$Count[1], format = "d", big.mark = ","), "transactions )\n",
      "• Peak Hour:", peak_hour$Hour, ":00 (", formatC(peak_hour$Count, format = "d", big.mark = ","), "transactions )\n\n",
      
      "Data Quality:\n",
      "• Missing Entrance Times:", formatC(missing_entrance, format = "d", big.mark = ","), 
      "(", round(100 * missing_entrance / nrow(data), 1), "%)\n",
      "• Missing Exit Times:", formatC(missing_exit, format = "d", big.mark = ","), 
      "(", round(100 * missing_exit / nrow(data), 1), "%)\n\n",
      
      "Data Status: Ready for analysis and forecasting!"
    )
  })
  
  output$transaction_table <- DT::renderDataTable({
    data <- parking_data()
    
    if(nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    # Apply filters
    filtered_data <- data
    
    if (!is.null(input$data_date_range)) {
      filtered_data <- filtered_data %>%
        filter(Date >= input$data_date_range[1], 
               Date <= input$data_date_range[2])
    }
    
    if (!is.null(input$data_lots) && length(input$data_lots) > 0) {
      filtered_data <- filtered_data %>%
        filter(LotNumber %in% input$data_lots)
    }
    
    # Show most recent data first, limit for performance
    display_data <- filtered_data %>%
      arrange(desc(Date)) %>%
      head(1000) %>%
      select(Date, LotNumber, CardNumber, Hour, Weekday, EffectiveGroupNumber, NoEntry, NoExit, Overnight)
    
    DT::datatable(display_data, 
                  options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    order = list(list(0, 'desc'))
                  ),
                  filter = 'top',
                  caption = paste("Showing", nrow(display_data), "most recent transactions"))
  })
  
  # Download Handler
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste("parking_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- parking_data()
      
      if(nrow(data) == 0) {
        write.csv(data.frame(Message = "No data available"), file, row.names = FALSE)
        return()
      }
      
      # Apply same filters as the table
      filtered_data <- data
      
      if (!is.null(input$data_date_range)) {
        filtered_data <- filtered_data %>%
          filter(Date >= input$data_date_range[1], 
                 Date <= input$data_date_range[2])
      }
      
      if (!is.null(input$data_lots) && length(input$data_lots) > 0) {
        filtered_data <- filtered_data %>%
          filter(LotNumber %in% input$data_lots)
      }
      
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)