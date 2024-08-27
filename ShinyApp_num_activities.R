library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(shinydashboard)

# Set the CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Read the uploaded file using a relative path with data.table for efficiency
df <- fread('Clean_file.csv')

# Convert 'num_activities' to numeric
df$num_activities <- as.numeric(df$num_activities)

# Parse dates
df$date_start <- mdy(df$date_start)  # Parses dates in "month/day/year" format

# Debugging information
print("Data loaded successfully")
print(df)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Plot Data"),
  dashboardSidebar(
    selectInput("chart_type", "Select Chart Type",
                choices = list("Line Chart" = "line", 
                               "Bar Chart" = "bar")),
    selectInput("agg_level", "Choose Aggregation Level",
                choices = list("Days" = "day", 
                               "Weeks" = "week",
                               "Months" = "month", 
                               "Years" = "year")),
    selectInput("var_select", "Select Variable",
                choices = setdiff(colnames(df), c("ID", "date_start"))), 
    selectInput("community_select", "Select Community",
                choices = unique(df$ID), multiple = TRUE),
    
    selectInput("line_color", "Select Line Color", choices = c("Blue", "Red", "Green", "Black"), selected = "Blue"),
    selectInput("line_type", "Select Line Type", choices = c("Solid" = "solid", "Dashed" = "dashed"), selected = "Solid"),
    checkboxInput("exclude_bots", "Exclude bots?", TRUE),
    actionButton("plot", "Generate Plot"),
    tags$div(style = "text-align: center; margin-top: 10px;", 
             downloadButton('downloadOriginal', 'Download Original Data')),
    tags$div(style = "text-align: center; margin-top: 10px;", 
             downloadButton('downloadFiltered', 'Download Filtered Data'))
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot1"), width = 12)
    )
  )
)

server <- function(input, output) {
  
  # Reactive function to filter data
  get_filtered_data <- reactive({
    req(input$plot)
    
    temp_df <- df
    
    # Print the selected communities
    cat("Selected communities: ", input$community_select, "\n")
    
    if (!is.null(input$community_select)) {
      temp_df <- temp_df[ID %in% input$community_select]
    }
    
    temp_df[, time := floor_date(date_start, input$agg_level)]
    
    # Keep the original num_activities values without any summarization
    result <- temp_df[, .(time, num_activities, ID)]
    
    # Debugging: Print filtered data
    cat("Filtered data preview:\n")
    print(head(result))  # Print the first few rows of the filtered data
    
    # Debugging: Print the summary of num_activities
    cat("Summary of num_activities:\n")
    print(summary(result$num_activities))
    
    result
  })
  
  # Render plot
  output$plot1 <- renderPlotly({
    data <- get_filtered_data()
    
    # Debugging: Print the data used for plotting
    cat("Data used for plotting:\n")
    print(head(data))
    
    # Check if the data is empty
    if (nrow(data) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    # Ensure 'time' is a Date object
    data$time <- as.Date(data$time)
    
    # Plotting with explicit grouping to prevent incorrect aggregation
    if (input$chart_type == "bar") {
      plot <- ggplot(data, aes(x = time, y = num_activities, fill = ID, 
                               text = paste("Time:", time, "<br>Community:", ID, "<br>Number of Activities:", num_activities))) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(x = "Time", y = "Number of Activities", fill = "Community")
    } else {
      plot <- ggplot(data, aes(x = time, y = num_activities, color = ID, group = interaction(ID),
                               text = paste("Date:", time, "<br>Community:", ID, "<br>Number of Activities:", num_activities))) +
        geom_line(linetype = input$line_type) +
        theme_minimal() +
        labs(x = "Time", y = "Number of Activities", color = "Community") +
        scale_x_date(date_labels = "%Y", expand = c(0, 0))
    }
    
    # Convert the ggplot to plotly, with the specified tooltip
    ggplotly(plot, tooltip = "text")
  })
  
  
  # Download handlers remain unchanged
  output$downloadOriginal <- downloadHandler(
    filename = function() {
      paste("original-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(get_filtered_data(), file, row.names = FALSE)
    }
  )
}



shinyApp(ui = ui, server = server)
