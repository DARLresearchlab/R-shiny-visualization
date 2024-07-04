# install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='digitalassetresearchlab', token='51095528F4C94E6E70E5979A8EDE961D', secret='0gx7AfIQP3VkDtYwR24gTz3wxDf+iqciIPiMraVG')
# rsconnect::configureApp("OSS-Tracer-App", size="xlarge") ## Not allowed for free version

# library(tidyverse)
library(shiny)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(shinydashboard)

## Uncomment when running on a local machine; comment when deploying - otherwise gives error status 1
# setwd('/Users/mariiapetryk/App')

# df <- read_csv("Data_AllYears_Merged.csv")
# df <- read_csv("Data_AllYears_Merged_with core and bot_v02.csv")
# df <- read_csv("Data_AllYears_Merged_with core and bot_v03_aggr.csv")
df <- read_csv("Data_AllYears_Merged_aggr_all_coins.csv")

df$date1 <- as.Date(df$date1, format="%m/%d/%y")
choices_communities=unique(df$CryptoID)

# df[df$CryptoID==choices_communities[2],]
df %>%
  filter(CryptoID %in% choices_communities[1])

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Plot Data"),
  dashboardSidebar(
    selectInput("agg_level", "Choose Aggregation Level",
                choices = list("Days" = "day", 
                               "Weeks" = "week",
                               "Months" = "month", 
                               "Years" = "year")),
    selectInput("var_select", "Select Variable",
                # choices = setdiff(colnames(df), c("date1", "GhRepo", "GhRepo_2", "actor_login", "Year", "actor_id", "release_payload"))),
                choices = setdiff(colnames(df), c("date1", "Year",'is_core_developer','is_user_bot', 'CryptoID'))),
    
    selectInput("community_select", "Select Community",
                choices = choices_communities),
                
    dateRangeInput("date_range", "Select Date Range", 
                   start = min(df$date1), 
                   end = max(df$date1),
                   format = "mm/dd/yyyy"),
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
    box(plotlyOutput("plot1"), width = 12)
  )
)

# Define server logic

server <- function(input, output) {
  
  get_filtered_data <- reactive({
    temp_df <- df
    if (input$exclude_bots) {
      temp_df <- temp_df %>% filter(is_user_bot == 0)
    }
    
    temp_df<-temp_df %>%
      filter(CryptoID %in% input$community_select)
    
    temp_df %>%
      filter(date1 >= input$date_range[1] & date1 <= input$date_range[2]) %>%
      mutate(time = floor_date(date1, input$agg_level)) %>%
      group_by(time) %>%
      summarise(!!input$var_select := sum(!!sym(input$var_select)))

  }
)
  
  output$plot1 <- renderPlotly({
    req(input$plot)
    
    data <- get_filtered_data()
    
    plot <- ggplot(data, aes(x = time, y = !!sym(input$var_select), group = 1,
                             text = paste("Date:", time, "<br>Value:", !!sym(input$var_select)))) +
      geom_line(color = input$line_color, linetype = input$line_type) +
      theme_minimal() +
      labs(x = "Time", y = input$var_select)
    
    ggplotly(plot, tooltip = "text")
  })
  
  # Original data download
  output$downloadOriginal <- downloadHandler(
    filename = function() {
      paste("original-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Filtered data download
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(get_filtered_data(), file, row.names = FALSE)
    }
  )
}
# .rs.files.restoreBindings()
# list.files(pattern = "*.csv")
shinyApp(ui = ui, server = server)

