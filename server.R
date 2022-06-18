#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DBI)
library(dplyr)
library(lubridate)
library(plotly)


# SQL Server Connection ---------------------------------------------------------

azure_credentials <- read.table("KEY-FILE.txt", sep = ",", header = FALSE)
uid <- azure_credentials[1,1]
pwd <- azure_credentials[1,2]
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};", 
                 server = "redkai11-superstore.database.windows.net", database = "superstore", 
                 uid = uid, pwd = pwd, timeout = 10)
to_date <- '2020-05-01'


# Helper Functions      ---------------------------------------------------------
color_net_Change <- function(x) {
  result <- x
  
  if(result >=0 ) { 
    result <- paste("<span style= \"background-color: #e3f4e3; border-radius: 50px; padding: 3px 10px; color:#69c669\"> +", abs(result), "% </span>")
  } else{
    result <- paste("<span style= \"background-color : #f3e2e2; border-radius: 50px; padding: 3px 10px; color:#cc7a7a\"> -", abs(result), "% </span>")
  }
  return(result)
}

number_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(round(x/1e3, 2)), "k"),
    x < 1e9 ~ paste0(as.character(x/1e6), "m")
  )
}

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}



shinyServer(function(input, output, session) {
    
  filtered_deals <- eventReactive(input$search, {
    req(input$fromDate)
    query <- paste("SELECT * FROM [dbo].[orders] WHERE [Order Date] >= '", input$fromDate, "' AND [Order Date] <= '", input$toDate,"'", sep = "")
    filtered_deals <- dbGetQuery(con, query)
  })
  
  output$analysis <- renderUI({
    req(input$filtered_deals)
    items_list <- if(nrow(filtered_deals()) > 0){
      DetailsList(items = filtered_deals())
    } else {
      p("No matching transactions.")
    }
    
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Sales deals details", block = TRUE),
      div(style="max-height: 500px; overflow: auto", items_list)
    )
  })
  
  
  overview_orders_filtered <- reactive({
    req(input$overview_filter_date)
    from_date <- as.Date(to_date) - as.numeric(input$overview_filter_date)
    query <- paste("SELECT * FROM [dbo].[orders] WHERE [Order Date] >= '", from_date, "' AND [Order Date] <= '", to_date,"'", sep = "")
    overview_orders_filtered <- dbGetQuery(con, query)
  })
  
  overview_returns_filtered <- reactive({
    req(input$overview_filter_date)
    from_date <- as.Date(to_date) - as.numeric(input$overview_filter_date)
    query <- paste("SELECT * FROM [dbo].[orders], [dbo].[Returns]",
                   "WHERE [dbo].[orders].[Order ID] = [dbo].[Returns].[Order ID] AND [Order Date] >= '",
                   from_date, "' AND [Order Date] <= '", to_date,"'",
                   " AND [dbo].[Returns].[Returned] = 'yes'", sep = "")
    overview_returns_filtered <- dbGetQuery(con, query)
  })
  
  output$customers <- renderText({
    req(overview_orders_filtered)
    number_formatter(length(unique(overview_orders_filtered()$`Customer ID`)))
  })
  
  output$revenue <- renderText({
    req(overview_orders_filtered)
    paste0("$", number_formatter(sum(overview_orders_filtered()$`Sales`)))
  })
  
  output$orders <- renderText({
    req(overview_orders_filtered)
    number_formatter(nrow(overview_orders_filtered()))
  })
  
  output$returns <- renderText({
    req(overview_returns_filtered)
    number_formatter(nrow(overview_returns_filtered()))
  })
  
  overview_orders_filtered_previous_date_range <- reactive({
    req(input$overview_filter_date)
    from_date <- as.Date(to_date) - 2 * as.numeric(input$overview_filter_date)
    to_date <- as.Date(to_date) - as.numeric(input$overview_filter_date)
    query <- paste("SELECT * FROM [dbo].[orders]",
                   "WHERE [Order Date] >= '",
                   from_date, "' AND [Order Date] <= '", to_date,"'", sep = "")
    overview_orders_filtered_previous_date_range <- dbGetQuery(con, query)
  })
  
  overview_returns_filtered_previous_date_range <- reactive({
    req(input$overview_filter_date)
    from_date <- as.Date(to_date) - 2 * as.numeric(input$overview_filter_date)
    to_date <- as.Date(to_date) - as.numeric(input$overview_filter_date)
    query <- paste("SELECT * FROM [dbo].[orders], [dbo].[Returns]",
                   "WHERE [dbo].[orders].[Order ID] = [dbo].[Returns].[Order ID] AND [Order Date] >= '",
                   from_date, "' AND [Order Date] <= '", to_date,"'",
                   " AND [dbo].[Returns].[Returned] = 'yes'", sep = "")
    overview_returns_filtered_previous_date_range <- dbGetQuery(con, query)
  })
  
  output$customers_change <- renderText({
    req(overview_orders_filtered)
    req(overview_orders_filtered_previous_date_range)
    previous <- length(unique(overview_orders_filtered_previous_date_range()$`Customer ID`))
    current <- length(unique(overview_orders_filtered()$`Customer ID`))
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  output$revenue_change <- renderText({
    req(overview_orders_filtered)
    req(overview_orders_filtered_previous_date_range)
    previous <- sum(overview_orders_filtered_previous_date_range()$Sales)
    current <- sum(overview_orders_filtered()$Sales)
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  output$orders_change <- renderText({
    req(overview_orders_filtered)
    req(overview_orders_filtered_previous_date_range)
    previous <- nrow(overview_orders_filtered_previous_date_range())
    current <- nrow(overview_orders_filtered())
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  
  output$returns_change <- renderText({
    req(overview_returns_filtered)
    req(overview_returns_filtered_previous_date_range)
    previous <- nrow(overview_returns_filtered_previous_date_range())
    current <- nrow(overview_returns_filtered())
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
    
  
  report_filtered <- reactive({
    req(input$report_type)
    req(input$report_filter_date)
    from_date <- as.Date(to_date) - as.numeric(input$report_filter_date)
    query <- paste("SELECT [Order Date], sum([", input$report_type,"])",
                   " FROM [dbo].[orders] WHERE [Order Date] >= '", from_date, "' AND [Order Date] <= '", to_date,"'",
                   " GROUP BY [Order Date]", sep = "")
    report_filtered <- dbGetQuery(con, query)
    report_filtered$'Order Date' <- lubridate::ymd(report_filtered$'Order Date')
    colnames(report_filtered) <- c("Date", input$report_type)
    return(report_filtered)
  })
  
  output$report <- renderPlotly({
    req(report_filtered)
    req(input$report_type)
    fig <- plot_ly() %>%
      add_trace(data = report_filtered(), type = 'scatter', mode = 'lines', 
                fill = 'tozeroy', x = ~Date, y = ~report_filtered()[,2], name = 'GOOG',
                fillcolor = 'rgba(168, 216, 234, 0.5)') %>%
      layout(showlegend = F, yaxis = list(title = input$report_type,
                                          zerolinecolor = '#ffff',
                                          zerolinewidth = 2,
                                          gridcolor = 'ffff'),
             xaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             plot_bgcolor='#ffffff')
  })
  
  
})
