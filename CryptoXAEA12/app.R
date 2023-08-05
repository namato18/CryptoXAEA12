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
      menuItem(text = "Predict", tabName = "predict", icon = icon("robot")),
      menuItem(text = "Additional Model Info", tabName = "create", icon = icon("magnifying-glass-chart")),
      menuItem(text = "Back-Test", tabName = "backtest", icon = icon("vial-circle-check")),
      menuItem(text = "General Info", tabName = "info", icon = icon("circle-info"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(
      theme = "flat_red"
    ),
    tabItems(
      tabItem(tabName = "predict",
        fluidRow(
          add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
          column(width = 6,
            box(title = "Inputs", solidHeader = TRUE, status = "danger", width = NULL,
                selectInput("selectCandlePred", "Select a Coin",choices = checkbox_list),
                selectInput("selectTimeframePred", "Select a Timeframe",choices = list("1 Hour" = "1hour",
                                                                                       "4 Hour" = "4hour",
                                                                                       "8 Hour" = "8hour",
                                                                                       "1 Day" = "1day")),
                actionButton("predictButtonPred","Predict", icon = icon('chart-simple'), class = class("danger"), style='padding:4px; width:100%')
                
            ),
            box(title = "Prediction", solidHeader = TRUE, status = "danger", width = NULL,
                infoBoxOutput("Prediction", width = 12),
              
            )
          ),

          column(width = 6,
            box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger", width = NULL,
                plotlyOutput('candlestickPlotPred')
            )
          )

        )
      ),
      
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
      tabItem(tabName = 'backtest',
              fluidRow(
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                img(src='logo.nobg.png', width = 175, height = 175, align = 'right' ),
                
                column(width = 6,
                box(title = "Back-Test Inputs", solidHeader = TRUE, status = "danger", width = NULL,
                    selectInput("selectBack", "Select a Coin",choices = checkbox_list),
                    selectInput("selectTimeframeBack", "Select a Timeframe",choices = list("1 Hour" = "1hour",
                                                                                       "4 Hour" = "4hour",
                                                                                       "8 Hour" = "8hour",
                                                                                       "1 Day" = "1day")),
                    selectInput("selectPredictBack", "Select Prediction",choices = list("Break High" = "BreakH",
                                                                                           "Break Low" = "BreakL",
                                                                                           "Break 1%" = "Break1",
                                                                                           "High" = "High",
                                                                                           "Low" = "Low",
                                                                                           "Close" = "Close")),
                    actionButton("backTestButton","Predict", icon = icon('chart-simple'), class = class("danger"), style='padding:4px; width:100%')
                    
                    ),
                box(title = "Histogram", solidHeader = TRUE, status = "danger", width = NULL,
                    plotOutput("histogram")
                )

                ),

                box(title = "Back-Test Metrics", solidHeader = TRUE, status = "danger",
                    infoBoxOutput("precision", width = 12),
                    infoBoxOutput("recall", width = 12),
                    infoBoxOutput("f1", width = 12),
                    
                    infoBoxOutput("rmse", width = 12),
                    infoBoxOutput("current.price", width = 12)
                    
                )
              )
        
      ),
      tabItem(tabName = "info",
              img(src='logo.nobg.png', width = 175, height = 175, align = 'right' ),
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
  
  output$candlestickPlotPred = renderPlotly(LivePlot(input$selectCandlePred, input$selectTimeframePred))
  
  
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
    predict.hlc(input$selectCandle, input$selectTimeframe, "detail")
    predict.blbh(input$selectCandle, input$selectTimeframe, "detail")
    predict.target(input$selectCandle, input$selectTimeframe, "detail")
    
    
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

    
  })
  
  observeEvent(input$backTestButton, {
    BackTest(input$selectBack, input$selectTimeframeBack, input$selectPredictBack)
    
    if(input$selectPredictBack == "High" |input$selectPredictBack == "Low" |input$selectPredictBack == "Close"){
      hide("precision")
      hide("recall")
      hide("f1")
      shinyjs::show("rmse")
      shinyjs::show("current.price")
      hide("histogram")
    }else{
      shinyjs::show("precision")
      shinyjs::show("recall")
      shinyjs::show("f1")
      shinyjs::show("histogram")
      hide("rmse")
      hide("current.price")
    }
    
    output$precision = renderInfoBox({
      infoBox("Precision", precision, icon = icon("arrow-trend-up"), color = "red")
    })
    output$recall = renderInfoBox({
      infoBox("recall", recall, icon = icon("arrow-trend-up"), color = "red")
    })
    output$f1 = renderInfoBox({
      infoBox("F1 Score", f1, icon = icon("arrow-trend-up"), color = "red")
    })
    output$rmse = renderInfoBox({
      infoBox("RMSE", rmse, icon = icon("arrow-trend-up"), color = "red")
    })
    output$current.price = renderInfoBox({
      infoBox("Current Price", current.price, icon = icon("arrow-trend-up"), color = "red")
    })
    output$histogram = renderPlot(p1)
    
  })
  
  observeEvent(input$predictButtonPred,{
    predict.hlc(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.blbh(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.target(input$selectCandlePred, input$selectTimeframePred, "no detail")
    MakePrediction(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc)
    
    if(pred.count <= 0){
      output$Prediction = renderInfoBox({
        infoBox("Do Not Buy", pred.count, icon = icon("arrow-trend-up"), color = "red")
      })
    }else if(pred.count == 1){
      output$Prediction = renderInfoBox({
        infoBox("Unclear Decision", pred.count, icon = icon("arrow-trend-up"), color = "yellow")
      })
    }else{
      output$Prediction = renderInfoBox({
        infoBox("Buy Signal", pred.count, icon = icon("arrow-trend-up"), color = "green")
      })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
