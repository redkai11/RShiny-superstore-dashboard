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
library(imola)
library(stringr)
library(dplyr)
library(readr)
library(leaflet)
library(glue)
library(purrr)
library(plotly)

# Global CSS Variables ---------------------------------------------------------

icon_style <- "width: 50px; height: 50px; border-radius: 50%; background-color: #b3dbf2; color : #0078d4;"

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


# UI components ---------------------------------------------------------
app_header <- flexPanel(
  id = "header",
  align_items = "center",
  flex = c(0, 1),
  div(class = "logo-wrapper",
      img(src = "superstore-logo.png", style = "width: 200px; margin: 0 20px")
      ),
  makeCard("", SearchBox.shinyInput("search", placeholder = "Search"), size = 6),
  makeCard("", CommandBar(items = header_commandbar_list), size  = 6), 
  style = ""
  #makeCard("", SearchBox.shinyInput("search", placeholder = "Search"), size = 6),
  # div(
  #   #Text(variant = "xLarge", "| Dashboard", style="color: gray;"),
  #   style = "margin-bottom: 10px;"),
)

# app_header <- div(
#   Stack(
#     horizontal = TRUE, 
#     makeCard("", img(src = "superstore-logo.png", style = "width: 200px; margin: 0 20px"), size = 2),
#     makeCard("", SearchBox.shinyInput("search", placeholder = "Search"), size = 8),
#     makeCard("", CommandBar(items = header_commandbar_list), size = 2),
#   )
# )

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Overview', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'shiny.fluent', url = '', key = 'repo', icon = 'GitGraph'),
      list(name = 'shiny.react', url = '', key = 'shinyreact', icon = 'GitGraph'),
      list(name = 'Appsilon', url = '', key = 'appsilon', icon = 'WebAppBuilderFragment')
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


# KPI Content ---------------------------------------------------------


customers_kpi <- Stack(
  horizontal = TRUE,
  tokens = list(childrenGap = 10),
  style = "display: flex; align-items: center;",
  div(
    style=icon_style,
    FontIcon(iconName = "AlignVerticalBottom", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Customer", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style= "margin-top: 16px;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem;",
          textOutput("customers")
        ),
        div(
          class = "net-change",
          h3("smt")
          #textOutput("customers_previous_date_range")
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
    FontIcon(iconName = "AlignVerticalBottom", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Revenue", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style= "margin-top: 16px;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem;",
          textOutput("revenue")
        ),
        div(
          class = "net-change",
          Text("+ 10%")
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
    FontIcon(iconName = "AlignVerticalBottom", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Orders", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style= "margin-top: 16px;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem;",
          textOutput("orders")
        ),
        div(
          class = "net-change",
          Text("+ 10%")
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
    FontIcon(iconName = "AlignVerticalBottom", style = "font-size: 1.5em; text-align: center; display: block; margin: 10%;")
  ),
  div(
    Stack(
      span("Total Returns", class = "kpi-title"),
      Stack(
        horizontal = TRUE,
        style= "margin-top: 16px;",
        div(
          class = "kpi-data",
          style = "color: black; font-size: 2.5rem;",
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

# Overview Content ---------------------------------------------------------


filters <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    DatePicker.shinyInput("fromDate", value = as.Date("2016/1/1"), label = "From date"),
    DatePicker.shinyInput("toDate", value = Sys.Date(), label = "To date"),
    PrimaryButton.shinyInput("search", text = "Search", style = "width: 150px; margin-top:30px")
  )
)

options <- list(
  list(key = "30", text = "Last 30 Days"),
  list(key = "7", text = "Last 7 Days"),
  list(key = "365", text = "Last 365 Days")
)

kpi <- Stack(
  horizontal = FALSE,
  Stack(
    horizontal = TRUE,
    class = "kpi-header-wrapper",
    makeCard("", span("Overview", class = "card-title"), size = 2),
    makeCard("", Dropdown.shinyInput("overview_filter_date", value = "30", options = options), size = 2, style = "margin-left : auto; margin-bottom: 16px;")
  ),
  Stack(
    horizontal = TRUE,
    class = "kpi-content-wrapper",
    style = "display: flex; justify-content: space-between;",
    tokens = list(childrenGap = 5),
    makeCard("", customers_kpi, size = 3),
    makeCard("", revenue_kpi, size = 3),
    makeCard("", orders_kpi, size = 3),
    makeCard("", returns_kpi, size = 3)
  )
)


report_options <- list(
  list(key = "Sales", text = "Sales"),
  list(key = "Profit", text = "Profit")
)


report <- Stack(
    horizontal = FALSE,
    class = "report-wrapper",
    tokens = list(childrenGap = 5),
    Stack(
      horizontal = TRUE,
      makeCard("", span("Report", class = 'card-title'), size = 2),
      makeCard("", Dropdown.shinyInput("report_type", value = "Sales", options = report_options), size = 2, style = "margin-left : 30px; width: fit-content"),
      makeCard("", Dropdown.shinyInput("report_filter_date", value = "30", options = options), size = 2, style = "margin-left : auto;")
    ),
    Stack(
      makeCard("", plotlyOutput("report"), size = 12)
    )

  )
  
                    
app_content <- div(
      makeCard("", kpi, size = 12, style = "background-color: transparent; margin-bottom: 16px; padding: 0 28px;"),
      # Stack(
      #   horizontal = FALSE,
      #   tokens = list(childrenGap = 5),
      #   makeCard("", filters, size = 12, style = "max-height: 600px;"),
      #   reactOutput("spinner"),
      #   uiOutput("analysis")
      # )
      makeCard("", report, size = 8, style = "background-color: transparent; margin-bottom: 28px; padding: 0 28px;")
    )

app_footer <- flexPanel(
  id = "footer",
  justify_content = 'space-between',
  gap = "20px",
  Text(variant = "medium", "Built with R.fluent and R.react", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "Data source: Data.World")
)

# Define UI for application that draws a histogram
shinyUI(
  gridPage(
    tags$head(includeCSS("www/styles.css")),
    template = "grail-left-sidebar",
    gap = "10px",
    rows = list(
      default = "70px 1fr 30px"
    ),
    columns = list(
      default = "minmax(240px, 12%) auto auto"
    ),
    header = app_header,
    sidebar = navigation,
    content = app_content,
    footer = app_footer
  )
)

