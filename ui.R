#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.fluent)
library(shiny.react)
library(shinyjs)
library(imola)
library(stringr)
library(dplyr)
library(readr)
library(leaflet)
library(glue)
library(purrr)
library(plotly)

# Global CSS Variables ---------------------------------------------------------

icon_style <- "width: 50px; height: 50px; border-radius: 50%; background-color: #b3dbf2; color : #0078d4; display: flex; justify-content: center; align-items: center;"

## Header Commander Bar

header_commandbar_list <- list(
  list(
    key = 'download',
    text = "Download data",
    iconProps = list(iconName = "Download")
  )
)


# Layout ---------------------------------------------------------

makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130;"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"


# UI components ---------------------------------------------------------
# app_header <- flexPanel(
#   id = "header",
#   align_items = "center",
#   flex = c(0, 1),
#   div(class = "search-bar-wrapper",
#       SearchBox.shinyInput("search", placeholder = "Search")
#       ),
#   makeCard("", CommandBar(items = header_commandbar_list), style = "padding : 14px;"), 
#   style = ""
# )

app_header <- fluidRow(
  style = "display: flex; align-items: center; margin: 12px 0",
  column(6, offset = 0, SearchBox.shinyInput("search", placeholder = "Search")),
  column(4, ""),
  column(2, CommandBar(items = header_commandbar_list))
)


navigation <- div(
  div(class = "logo-wrapper",
                 img(src = "superstore-logo.png", style = "width: 200px; margin: 0 20px;")),
  Nav(
    groups = list(
      list(links = list(
        
        div(class = "logo-wrapper",
            img(src = "superstore-logo.png", style = "max-width:100%; max-height:100%;")
        ),
        list(name = 'Overview', url = '', key = 'home', icon = 'Home'),
        list(name = "View Orders", url = '', key = 'viewOrdersNav', icon = 'AnalyticsReport'),
        list(name = 'Analysis', url = '', key = 'analysis', icon = 'AnalyticsReport')
      ))
    ),
    initialSelectedKey = 'home',
    styles = list(
      root = list(
        height = '100%',
        boxSizing = 'border-box',
        overflowY = 'auto'
      )
    )
  )
)
  



# KPI Content ---------------------------------------------------------


customers_kpi <- Stack(
  horizontal = TRUE,
  tokens = list(childrenGap = 10),
  style = "display: flex; align-items: center;",
  div(
    style=icon_style,
    FontIcon(iconName = "PeopleAdd", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Customers", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style = "display: flex; align-items: center;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem; display: block;",
          textOutput("customers")
          
        ),
        div(
          class = "net-change",
          htmlOutput("customers_change")
        )
      )
    )
  )
)


revenue_kpi <- Stack(
  horizontal = TRUE,
  tokens = list(childrenGap = 10),
  style = "display: flex; align-items: center;",
  div(
    style=icon_style,
    FontIcon(iconName = "Money", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Revenue", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style = "display: flex; align-items: center;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem; display: block;",
          textOutput("revenue")
          
        ),
        div(
          class = "net-change",
          htmlOutput("revenue_change")
        )
      )
    )
  )
)


orders_kpi <- Stack(
  horizontal = TRUE,
  tokens = list(childrenGap = 10),
  style = "display: flex; align-items: center;",
  div(
    style=icon_style,
    FontIcon(iconName = "ActivateOrders", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Orders", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style = "display: flex; align-items: center;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem; display: block;",
          textOutput("orders")
        ),
        div(
          class = "net-change",
          htmlOutput("orders_change")
        )
      )
    )
  )
)

returns_kpi <- Stack(
  horizontal = TRUE,
  tokens = list(childrenGap = 10),
  style = "display: flex; align-items: center;",
  div(
    style=icon_style,
    FontIcon(iconName = "Rotate90CounterClockwise", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Returns", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style = "display: flex; align-items: center;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem; display: block;",
          textOutput("returns")
          
        ),
        div(
          class = "net-change",
          htmlOutput("returns_change")
        )
      )
    )
  )
)

options <- list(
  list(key = "30", text = "Last 30 Days"),
  list(key = "7", text = "Last 7 Days"),
  list(key = "365", text = "Last 365 Days")
)

kpi <- Stack(
  horizontal = FALSE,
  fluidRow(
    column(12, Stack(
      horizontal = TRUE,
      class = "kpi-header-wrapper",
      makeCard("", div(
        span("Overview", class = "card-title"),
        TooltipHost(
          content = "Displays corresponding KPI value and change from the previous date range for the given date range.",
          delay = 0,
          FontIcon(iconName = "info", style = "margin: 0 12px; font-size: 16px;")
        )), size = 2),
      makeCard("", Dropdown.shinyInput("overview_filter_date", value = "30", options = options), size = 2, style = "margin-left : auto; margin-bottom: 16px;")
    ))
  ),
  fluidRow(
    column(3, div(class = "kpi-style", customers_kpi)),
    column(3, div(class = "kpi-style", revenue_kpi)),
    column(3, div(class = "kpi-style", orders_kpi)), 
    column(3, div(class = "kpi-style", returns_kpi))
  )
)


report_options <- list(
  list(key = "Sales", text = "Sales"),
  list(key = "Profit", text = "Profit")
)

# Report Content ---------------------------------------------------------

sales_line_plot <- fluidRow(
  class = "chart-wrapper",
  column(12, fluidRow(
    class = "chart-title-wrapper",
    column(1, span("Report", class = 'card-title')),
    column(2, Dropdown.shinyInput("report_type", value = "Sales", options = report_options)),
    column(6, ""),
    column(3, Dropdown.shinyInput("report_filter_date", value = "30", options = options))
  )),
  column(12, 
         plotlyOutput("line_chart", height = "325px")
         )
)

category_pie_chart <- fluidRow(
  class = "chart-wrapper",
  column(12, fluidRow(
    class = "chart-title-wrapper",
    column(1, span("Report", class = 'card-title')),
  )),
  column(12, 
         plotlyOutput("category_chart", height = "325px")
  )
)


recent_orders <- fluidRow(
  class = "chart-wrapper",
  column(12, fluidRow(
    class = "chart-title-wrapper",
    column(2, span("Recent Orders", class = 'card-title')),
  )),
  column(12, 
         column(12, uiOutput("recent_orders_table"))
  )
)

app_content <- fluidPage(
      fluidRow(
        style = "margin: 16px 0;",
        column(12, kpi)
      ),
      fluidRow(
        style= "margin: 16px 0;",
        column(8, sales_line_plot),
        column(4, category_pie_chart)
      ),
      fluidRow(
        style= "margin: 16px 0;",
        column(12, recent_orders)
      )
    )

app_footer <- flexPanel(
  id = "footer",
  justify_content = 'space-between',
  gap = "20px",
  Text(variant = "medium", "Built with R.fluent and R.react", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "Data source: Data.World")
)

view_orders <- fluidPage(
  fluidRow(
    column(12, 
          uiOutput("view_orders")        
           )
  )
)
# Define UI for application that draws a histogram
shinyUI(
  # Loading message
  
  fluidPage(
    tags$head(includeCSS("www/styles.css")),
    useShinyjs(),
    inlineCSS(appCSS),
    
    # div(id = "loading-content",
    #     h3("loading")
    #     ),
  
    # div(
    #   id = "main-content",
      fluidRow(
        column(2, navigation, style="background-color : white; background-clip : padding-box;"),
        column(10,
               column(12, app_header, style = "background-color : white; background-clip : padding-box;"),
               column(12, app_content))
      )
    #)
  )
)

