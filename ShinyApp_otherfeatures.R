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
df <- fread('FileName')

# Convert 'num_activities' to integer

# Print out the first few rows of the 'num_activities' column to check the conversion

print("Data structure after loading:")
print(str(df))
# Remove the specified columns
df <- df[, !c("actor_login", "GhRepo_2", "GhRepo")]

df$date1 <- as.Date(df$date1, format="%m/%d/%Y")

# Debugging information
print("Data loaded successfully")
print(head(df))


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
                choices = setdiff(colnames(df), 
                                  c("num_activities", "actor_id", "date", grep("^V", colnames(df), value = TRUE), "date1", "Year", 'is_user_bot', 'ID','date_start'))),
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
  
  get_filtered_data <- reactive({
    req(input$plot)
    
    temp_df <- df
    if (input$exclude_bots) {
      temp_df <- temp_df[is_user_bot == 0]
    }
    
    temp_df <- temp_df[ID %in% input$community_select]
    temp_df[, time := floor_date(date1, input$agg_level)]
    
    temp_df[[input$var_select]] <- as.numeric(temp_df[[input$var_select]])
    
    if (input$chart_type == "bar") {
      # Aggregate data by community
      result <- temp_df[, .(total_value = sum(get(input$var_select), na.rm = TRUE)), by = ID]
    } else {
      # Aggregate data by time and community
      result <- temp_df[, .(value = sum(get(input$var_select), na.rm = TRUE)), by = .(time, ID)]
    }
    
    # Debugging information
    print("Filtered data:")
    print(head(result))
    
    result
  })
  
  output$plot1 <- renderPlotly({
    data <- get_filtered_data()
    
    # Check if the data is empty
    if (nrow(data) == 0) {
      showNotification("No data available for the selected filters.", type = "error")
      return(NULL)
    }
    
    # Ensure 'time' is a Date object
    data$time <- as.Date(data$time)
    
    if (input$chart_type == "bar") {
      plot <- ggplot(data, aes(x = ID, y = total_value, fill = ID, 
                               text = paste("Community:", ID, "<br>Total Value:", total_value))) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Community", y = input$var_select, fill = "Community")
    } else {
      plot <- ggplot(data, aes(x = time, y = value, color = ID, group = ID,
                               text = paste("Date:", time, "<br>Value:", value, "<br>Community:", ID))) +
        geom_line(linetype = input$line_type) +
        theme_minimal() +
        labs(x = "Time", y = input$var_select, color = "Community") +
        scale_x_date(date_labels = "%Y", expand = c(0, 0))
    }
    
    ggplotly(plot, tooltip = "text")
  })
  
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

# Run the app
shinyApp(ui = ui, server = server)