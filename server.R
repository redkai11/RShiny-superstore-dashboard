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
library(shiny.fluent)
library(shiny.react)
library(promises)
library(future)
library(leaflet.extras)
plan(multisession)

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
  
  isSettingsPanelOpen <- reactiveVal(FALSE)
  output$settingsPanel <- renderReact({
    Panel(
      headerText = "Settings",
      isOpen = isSettingsPanelOpen(),
      fluidRow(
        style = "display : contents; margin-top: 16px;",
        column(7,numericInput("target_sales", "Change Target Sales:", 1000000, min = 1, max = 9999999)),
        column(3,
               style = "margin-top: 25px;",
               PrimaryButton.shinyInput("set_target_sales", text = "Set"))
      ),
      onDismiss = JS("function() { Shiny.setInputValue('hidePanel', Math.random()); }")
    )
  })
  observeEvent(input$setting, isSettingsPanelOpen(TRUE))
  observeEvent(input$hidePanel, isSettingsPanelOpen(FALSE))

  # Home  ---------------------------------------------------------
  
  overview_orders_filtered <- reactive({
    req(input$overview_filter_date)
    from_date <- as.Date(to_date) - as.numeric(input$overview_filter_date)
    query <- paste("SELECT * FROM [dbo].[orders] WHERE [Order Date] >= '", from_date, "' AND [Order Date] <= '", to_date,"'", sep = "")
    overview_orders_filtered <- dbGetQuery(con, query)
    # query <- c("SELECT * FROM [dbo].[orders] WHERE [Order Date] >= '?' AND [Order Date] <= '?'")
    # temp <- dbSendQuery(con, query)
    # dbBind(temp, list(from_date, to_date))
    # overview_orders_filtered <- dbFetch(temp)
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
  
  sales_target <- eventReactive(input$set_target_sales, {
    # TODO:
    # Alter table value
    req(input$target_sales)
    sales_target <- numeric(input$target_sales)
  })
  
  output$sales_target_chart <- renderPlotly({
    # TODO:
    # Create a new table and query
    # Change title according to the date
    
    query <- c("SELECT SUM(Sales) FROM QuaterlySales WHERE QuarterDate = '2020 Q2'")
    df <- dbGetQuery(con, query)
    current_sales <- df[1,1]
  
    target <- 1000000
    current_progress <- round(current_sales/target * 100)
    df <- data.frame(name = c("progress", ""), values = c(current_progress, 100 - current_progress))
    
    fig <- plot_ly(df, labels = ~name, values = ~values,
                   marker = list(colors = c('rgba(168, 216, 234, 0.5)', 'rgba(0,0,0,0)'),
                                 line = list(color = '#D0C9C0', width = 1)), sort = FALSE, textinfo = "none") %>%
      add_pie(hole = 0.5) %>%
      plotly::layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             margin = list(l = 60, r = 60)) %>%
      style(hoverinfo = 'none') %>%
      add_annotations(
        x=0.5,
        #y=1.5,
        text= paste(current_progress, "%"), 
        showarrow=F,
        font=list(size=15)
      )
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
  
  monthly_aggregation_df <- reactive({
    query <- c("SELECT DATEPART(month, [Order Date]) as Month, COUNT([Order ID]) as Frequency 
               FROM [dbo].[Orders] WHERE [Order Date] >= 2020 GROUP BY DATEPART(month, [Order Date])")
    df <- dbGetQuery(con, query)
  })
  
  output$monthly_orders_chart <- renderPlotly({
    req(monthly_aggregation_df())
    
    fig <- plot_ly(monthly_aggregation_df(), x = ~Month, y = ~Frequency, type = 'bar', color= I("#a8d8ea80"))
    fig <- fig %>% plotly::layout(title = "", xaxis = list(title = "Month"), yaxis = list(title = "# of Orders"))
  })
  
  states_aggregation_df <- reactive({
    states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
    df <- dbGetQuery(con, "SELECT [State] as name, count([Order ID]) as density FROM [dbo].[Orders] GROUP BY [State]")
    dfx <- data.frame(name = states$name, density = states$density)
    dfx <- dfx %>% left_join(df, by = c("name" = "name"))
    dfx[is.na(dfx)] <- 0
    dfx <- dfx[,c(1,3)]
    states$density <- dfx$density.y
    return(states)
  })
  
  output$orders_choropleth_map <- renderLeaflet({
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("Blues", domain = states$density, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Orders",
      states_aggregation_df()$name, states_aggregation_df()$density
    ) %>% lapply(htmltools::HTML)

    orders_choropleth_map <- leaflet(states_aggregation_df()) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addResetMapButton() %>%
      addPolygons(
        fillColor = ~pal(density),
        layerId = ~name,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "topright")
  })
  
  # TODO: make navbar selected to Orders
  
  orders_df <- reactiveValues(data = data.frame())
  
  observeEvent(input$search_button,{
    change_page("/orders", session = shiny::getDefaultReactiveDomain(), mode = "push")
    text <- input$search_content
    text <- str_split(text, ", ")
    text <- paste(text, collapse = " AND ")
    
    query <- paste("SELECT * FROM [dbo].[Orders] WHERE ", text, sep = "")
    orders_df$data <- dbGetQuery(con, query)
  })

  observeEvent(input$orders_choropleth_map_shape_click, {
    click <- input$orders_choropleth_map_shape_click
    query <- paste("SELECT * FROM [dbo].[Orders] WHERE [State] = '", click$id, "'", sep = "")
    orders_df$data <- dbGetQuery(con, query)
  })
  
  output$orders_table <- renderUI({
    req(orders_df)
    items_list <- if(nrow(orders_df$data) > 0){
      DetailsList(items = orders_df$data)
    } else {
      p("No matching transactions.")
    }
    
    Stack(
      tokens = list(childrenGap = 5),
      div(style="max-height: 1200px; overflow: auto", items_list)
    )
  })
  
  # View Products  ---------------------------------------------------------
# 

# 
#   products_df <- reactive({
#     if(values$starting) return(NULL)
#     query <- c("SELECT * FROM [dbo].[orders]")
#     
#     all_orders <- future(dbGetQuery(con, query))
#     products_df <- unique(all_orders[c("Product ID", "Category", "Sub-Category", "Product Name")])
#   })
# # 
#   output$products_table <- renderUI({
#     req(products_df())
#     items_list <- if(nrow(products_df()) > 0){
#       DetailsList(items = products_df())
#     } else {
#       p("No matching transactions.")
#     }
# 
#     Stack(
#       tokens = list(childrenGap = 5),
#       div(style="max-height: 1200px; overflow: auto", items_list)
#     )
#   })
#   
  # View Orders  ---------------------------------------------------------

  orders_group_by_category_df <- reactive({
    query <- c("SELECT [Category], COUNT(*) as Frequency 
               FROM [dbo].[Orders] WHERE [Order Date] >= 2020 GROUP BY [Category]")
    df <- dbGetQuery(con, query)
  })
  
  output$category_chart <- renderPlotly({
    req(orders_group_by_category_df())
    category_df <- orders_group_by_category_df()

    colors <- c("#5EB1BF", "#D84727", "#EF7B45", "#CDEDF6", "#042A2B")
    fig <- plot_ly(category_df, labels = ~Category,
                   values = ~Frequency, textposition = "inside",
                   insidetextfont = list(color = "white"), hoverinfo = "text",
                   marker = list(colors = colors,
                                 line = list(color = "white", width = 1)))
    fig <- fig %>% plotly::layout(legend = list(margin = list(l = 30, r = 30)))
    fig <- fig %>% add_pie(hole = 0.5)
  })
  

  
})
