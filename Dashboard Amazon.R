# Set the Working Directory
setwd("C:/Users/Mubeen Ahmad/Downloads")

# Pakkages used to Dashboard
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)

# Sample data
set.seed(123)
data <- tibble(
  date = rep(seq(as.Date("2025-07-01"), as.Date("2025-07-31"), by = "day"), 4),
  metric = rep(c("Clicks", "Spend", "Orders", "Sales"), each = 31),
  value = c(
    runif(31, 200, 400),  # Clicks
    runif(31, 100, 600),  # Spend
    runif(31, 10, 60),    # Orders
    runif(31, 500, 2500)  # Sales
  )
)

# UI
ui <- fluidPage(
  titlePanel("Campaign Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("metrics", "Select Metrics", 
                         choices = c("Clicks", "Spend", "Orders", "Sales"),
                         selected = c("Clicks", "Spend", "Orders", "Sales")),
      dateRangeInput("daterange", "Select Date Range",
                     start = min(data$date), end = max(data$date))
    ),
    mainPanel(
      plotlyOutput("linePlot", height = "500px")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(metric %in% input$metrics,
             date >= input$daterange[1],
             date <= input$daterange[2])
  })
  
  output$linePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = value, color = metric)) +
      geom_line(size = 1) +
      labs(x = "Date", y = "Value", color = "Metric") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run the app
shinyApp(ui, server)

