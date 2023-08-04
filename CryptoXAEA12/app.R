library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(plotly)
library(aws.s3)
library(riingo)
library(shinyWidgets)
library(stringr)
library(shinycssloaders)
library(quantmod)
library(CandleStickPattern)
library(shinybusy)

riingo_set_token("6fbd6ce7c9e035489f6238bfab127fcedbe34ac2")

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
      menuItem(text = "Test", tabName = "create", icon = icon("house")),
      menuItem(text = "More Info", tabName = "info", icon = icon("circle-info"))
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
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                img(src='logo.nobg.png', width = 175, height = 175, align = 'right' ),
                setBackgroundImage(
                  # color = "black",
                  src = "tower.jpg",
                  shinydashboard = TRUE
                ),
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "danger", width = NULL,
                           selectInput("selectCandle", "Select a Coin",choices = checkbox_list),
                           selectInput("selectTimeframe", "Select a Timeframe",choices = list("1 Hour" = "1hour",
                                                                                              "4 Hour" = "4hour",
                                                                                              "8 Hour" = "8hour",
                                                                                              "1 Day" = "1day")),
                           # actionBttn("predictButton",
                           #            "Predict",
                           #            icon = icon('chart-simple'),
                           #            style = "jelly",
                           #            color = "danger",
                           #            block = TRUE),
                           actionButton("predictButton","Predict", icon = icon('chart-simple'), class = class("danger"), style='padding:4px; width:100%')
                       ),
                       box(title = "Predicted High, Low, Close", solidHeader = TRUE, status = "danger", width = NULL,
                           
                           br(),
                           br(),
                           infoBoxOutput("High", width = 12),
                           infoBoxOutput("Low", width = 12),
                           infoBoxOutput("Close", width = 12)
                       ),
                       box(title = "Predicted Break High/Low", solidHeader = TRUE, status = "danger", width = NULL,
                           infoBoxOutput("BreakHigh", width = 12),
                           infoBoxOutput("BreakLow", width = 12)
                           
                       ),
                       box(title = "Predicted 1% Increase", solidHeader = TRUE, status = "danger", width = NULL,
                           infoBoxOutput("oneperc", width = 12)
                       )
                ),
                
                column(width=6,
                       box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger", width = NULL,
                           plotlyOutput('candlestickPlot')
                       )
                )
                
                
                
                
                
                
              )
      ),
      tabItem(tabName = "info",
              strong(h1("Additional Information")),
              br(),
              br(),
              box(title = "Method Used", solidHeader = TRUE, status = "danger",
                  strong("Machine Learning Model Used:"),
                  "XGBoost",
                  br(),
                  br(),
                  strong("What is XGBoost?:"),
                  "XGBoost is a boosting algorithm that uses bagging, which trains multiple decision trees and then combines the results.
                  It allows XGBoost to learn more quickly than other algorithms but also gives it an advantage in situations with many features to consider.",
                  br(),
                  br(),
                  strong("A Combination of Models:"),
                  "This application uses multiple predictions that feed into a final decision. 
                  The predictions include:",
                  br(),
                  br(),
                  "- Predict the next high, low and close",
                  br(),
                  "- Predict if the current candle will break the previous high/low",
                  br(),
                  "- Predict if the current candle will break a 1% high",
                  br(),
                  br(),
                  "The purpose of having multiple models is to confirm buy signals. The more models that we can have agreeing with eachother,
                   the better chance we have of our prediction being true."
              ),
              box(title = "Future Additions", solidHeader = TRUE, status = "danger",
                  strong("Further Testing and Parameter Tuning:"),
                  "All models can be improved through further testing and parameter tuning. parameter tuning is the method of 
                adjusting model parameters and inputs to obtain optimal results.",
                  br(),
                  br(),
                  strong("ChatGPT Integration:"),
                  "Want ChatGPT added to the application for the user to have access to? This can be integrated but will just take some time.",
                  br(),
                  br(),
                  strong("Automated Trading:"),
                  "Automated trading can be set up to utilize whichever trading platform the user prefers!"
                  
              )
              
      )
    )
  )
  
  
)

# Define server logic
server <- function(input, output) {
  
  output$High = NULL
  output$Low = NULL
  output$Close = NULL
  output$oneperc = NULL
  output$BreakHigh = NULL
  output$BreakLow = NULL
  
  
  observeEvent(input$selectCandle, {
    output$candlestickPlot = renderPlotly(LivePlot(input$selectCandle, input$selectTimeframe))
  })
  
  observeEvent(input$selectTimeframe, {
    output$candlestickPlot = renderPlotly(LivePlot(input$selectCandle, input$selectTimeframe))
  })
  
  observeEvent(input$predictButton, {
    predict.hlc(input$selectCandle, input$selectTimeframe)
    predict.blbh(input$selectCandle, input$selectTimeframe)
    predict.target(input$selectCandle, input$selectTimeframe)
    
    
    output$High = renderInfoBox({
      infoBox("Predicted High", text.high, icon = icon("arrow-trend-up"), color = "red")
    })
    output$Low = renderInfoBox({
      infoBox("Predicted Low", text.low, icon = icon("arrow-trend-up"), color = "red")
    })
    output$Close = renderInfoBox({
      infoBox("Predicted Close", text.close, icon = icon("arrow-trend-up"), color = "red")
    })
    
    output$BreakHigh = renderInfoBox({
      infoBox("Confidence to Break Previous High", text.bh, icon = icon("arrow-trend-up"), color = "red")
    })
    output$BreakLow = renderInfoBox({
      infoBox("Confidence to Break Previous Low", text.bl, icon = icon("arrow-trend-up"), color = "red")
    })
    
    output$oneperc = renderInfoBox({
      infoBox("Confidence to Break 1% Increase", text.perc1, icon = icon("arrow-trend-up"), color = "red")
    })
    # output$Low = renderInfoBox({
    #   infoBox("Predicted Low", predict.hlc(input$selectCandle, input$selectTimeframe, "Low"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$Close = renderInfoBox({
    #   infoBox("Predicted Close", predict.hlc(input$selectCandle, input$selectTimeframe, "Close"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$High = renderInfoBox({
    #   infoBox("Predicted High", predict.hlc(input$selectCandle, input$selectTimeframe, "High"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$Low = renderInfoBox({
    #   infoBox("Predicted Low", predict.hlc(input$selectCandle, input$selectTimeframe, "Low"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$Close = renderInfoBox({
    #   infoBox("Predicted Close", predict.hlc(input$selectCandle, input$selectTimeframe, "Close"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$BreakHigh = renderInfoBox({
    #   infoBox("Confidence Break High", predict.blbh(input$selectCandle, input$selectTimeframe, "BreakH"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$BreakLow = renderInfoBox({
    #   infoBox("Confidence Break Low", predict.blbh(input$selectCandle, input$selectTimeframe, "BreakL"), icon = icon("arrow-trend-up"), color = "red")
    # })
    # output$oneperc = renderInfoBox({
    #   infoBox("Confidence Break 1% High", predict.target(input$selectCandle, input$selectTimeframe), icon = icon("arrow-trend-up"), color = "red")
    # })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
