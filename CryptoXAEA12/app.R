library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(plotly)
library(aws.s3)
library(riingo)

Sys.setenv(TZ="UTC")
source("Funcs.R")

str1 = readRDS("tickers/str1.rds")

checkbox_list = setNames(str1, str1)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Crypto",
    mainText = 'Predictor',
    badgeText = "v1.0"
  ),
  titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Test", tabName = "create", icon = icon("house"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(
      theme = "flat_red"
    ),
    tabItems(
      tabItem(tabName = "create",
              fluidRow(
                setBackgroundImage(
                  # color = "black",
                  src = "tower.jpg",
                  shinydashboard = TRUE
                ),
                box(title = "Predicted High, Low, Close", solidHeader = TRUE, status = "danger",
                    infoBoxOutput("High", width = 6),
                    infoBoxOutput("Low", width = 6),
                    infoBoxOutput("Close", width = 6)
                    ),
                box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger",
                    selectInput("selectCandle", "Select a Coin",choices = checkbox_list),
                    selectInput("selectTimeframe", "Select a Timeframe",choices = list("1 Hour" = "1hour",
                                                                                       "4 Hour" = "4hour",
                                                                                       "8 Hour" = "8hour",
                                                                                       "1 Day" = "1day")),
                    
                    plotlyOutput('candlestickPlot')
                )
              ))
    )
  )
  

)

# Define server logic
server <- function(input, output) {
  observeEvent(input$selectCandle, {
    output$candlestickPlot = renderPlotly(LivePlot(input$selectCandle, input$selectTimeframe))
  })

observeEvent(input$selectTimeframe, {
  output$candlestickPlot = renderPlotly(LivePlot(input$selectCandle, input$selectTimeframe))
})

}

# Run the application 
shinyApp(ui = ui, server = server)