#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DBI)
library(dplyr)
library(lubridate)


# SQL Server Connection ---------------------------------------------------------


azure_credentials <- read.table("KEY-FILE.txt", sep = ",", header = FALSE)
uid <- azure_credentials[1,1]
pwd <- azure_credentials[1,2]
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};", 
                 server = "redkai11-superstore.database.windows.net", database = "superstore", 
                 uid = uid, pwd = pwd, timeout = 10)
to_date <- as.Date('2020-05-01')


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
    x < 1e6 ~ paste0(as.character(round(x/1e3, 1)), "k"),
    x < 1e9 ~ paste0(as.character(x/1e6), "m")
  )
}


source('ui.R')

# server      ---------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # Navigation  ---------------------------------------------------------
  
  router$server(input, output, session)

  # Home  ---------------------------------------------------------
  
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
    req(overview_orders_filtered())
    number_formatter(length(unique(overview_orders_filtered()$`Customer ID`)))
  })
  
  output$revenue <- renderText({
    req(overview_orders_filtered())
    paste0("$", number_formatter(sum(overview_orders_filtered()$`Sales`)))
  })
  
  output$orders <- renderText({
    req(overview_orders_filtered())
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
    req(overview_orders_filtered())
    req(overview_orders_filtered_previous_date_range)
    previous <- length(unique(overview_orders_filtered_previous_date_range()$`Customer ID`))
    current <- length(unique(overview_orders_filtered()$`Customer ID`))
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  output$revenue_change <- renderText({
    req(overview_orders_filtered())
    req(overview_orders_filtered_previous_date_range)
    previous <- sum(overview_orders_filtered_previous_date_range()$Sales)
    current <- sum(overview_orders_filtered()$Sales)
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  output$orders_change <- renderText({
    req(overview_orders_filtered())
    req(overview_orders_filtered_previous_date_range)
    previous <- nrow(overview_orders_filtered_previous_date_range())
    current <- nrow(overview_orders_filtered())
    result <- round((current - previous)/previous * 100)
    return(color_net_Change(result))
  })
  
  
  output$returns_change <- renderText({
    req(overview_returns_filtered())
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
  
  output$line_chart <- renderPlotly({
    req(report_filtered())
    req(input$report_type)
    fig <- plot_ly(height = 325) %>%
      add_trace(data = report_filtered(), type = 'scatter', mode = 'lines', 
                fill = 'tozeroy', x = ~Date, y = ~report_filtered()[,2], name = 'GOOG',
                fillcolor = 'rgba(168, 216, 234, 0.5)') %>%
      plotly::layout(yaxis = list(title = input$report_type, zerolinecolor = 'white',zerolinewidth = 2, gridcolor = 'white'),
             xaxis = list(title = "Date", zerolinecolor = 'white', zerolinewidth = 2, gridcolor = 'white'), 
             plot_bgcolor='#ffffff', hovermode = "x unified")
  })
  
  output$category_chart <- renderPlotly({
    req(overview_orders_filtered())
    category_df <- overview_orders_filtered() %>% count(Category, sort = TRUE)
    
    colors <- c("#5EB1BF", "#D84727", "#EF7B45", "#CDEDF6", "#042A2B")
    fig <- plot_ly(category_df, labels = ~Category, 
                   values = ~n, textposition = "inside", 
                   insidetextfont = list(color = "white"), hoverinfo = "text",
                   marker = list(colors = colors, 
                                 line = list(color = "white", width = 1)))
    fig <- fig %>% plotly::layout(legend = list(margin = list(l = 30, r = 30)))
    fig <- fig %>% add_pie(hole = 0.6)
  })
  
  
  recent_orders_filtered <- reactive({
    query <- paste("SELECT * FROM [dbo].[orders] WHERE [Order Date] >= '", to_date - 7, "' AND [Order Date] <= '", to_date,"'", sep = "")
    recent_orders_filtered <- dbGetQuery(con, query)
  })
  
  output$recent_orders_table <- renderUI({
    req(recent_orders_filtered())
    items_list <- if(nrow(recent_orders_filtered()) > 0){
      DetailsList(items = recent_orders_filtered())
    } else {
      p("No matching transactions.")
    }
    
    Stack(
      tokens = list(childrenGap = 5),
      div(style="max-height: 400px; overflow: auto", items_list)
    )
  })
  
  # View Products  ---------------------------------------------------------

  values <- reactiveValues(starting = TRUE)

  session$onFlushed(function() {
    values$starting <- FALSE
  })

  products_df <- reactive({
    if(values$starting) return(NULL)
    query <- c("SELECT * FROM [dbo].[orders]")
    all_orders <- dbGetQuery(con, query)
    products_df <- unique(all_orders[c("Product ID", "Category", "Sub-Category", "Product Name")])
  })

  output$products_table <- renderUI({
    req(products_df())
    items_list <- if(nrow(products_df()) > 0){
      DetailsList(items = products_df())
    } else {
      p("No matching transactions.")
    }

    Stack(
      tokens = list(childrenGap = 5),
      div(style="max-height: 1200px; overflow: auto", items_list)
    )
  })
  
  # View Orders  ---------------------------------------------------------
  
  orders_df <- reactive({
    if(values$starting) return(NULL)
    query <- c("SELECT * FROM [dbo].[orders]")
    orders_df <- dbGetQuery(con, query)
  })

  output$orders_table <- renderUI({
    req(orders_df())
    items_list <- if(nrow(orders_df()) > 0){
      DetailsList(items = orders_df())
    } else {
      p("No matching transactions.")
    }

    Stack(
      tokens = list(childrenGap = 5),
      div(style="max-height: 1200px; overflow: auto", items_list)
    )
  })
  
})
