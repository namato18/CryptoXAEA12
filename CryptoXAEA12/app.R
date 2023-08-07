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
library(shinythemes)

riingo_set_token("6fbd6ce7c9e035489f6238bfab127fcedbe34ac2")

Sys.setenv(TZ="UTC")
source("Funcs.R")

str1 = readRDS("tickers/str1.rds")

checkbox_list = setNames(str1, str1)

# Define UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Crypto Predictor",
    mainText = img(src='fulllogo-removebg.png', width = 70, height = 70),
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
    # shinyDashboardThemes(
    #   theme = "flat_red"
    # ),
    tabItems(
      tabItem(tabName = "predict",
        fluidPage(
          theme = shinytheme("cyborg"),
          add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
          column(width = 6,
            box(title = "Inputs", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                selectInput("selectCandlePred", "Select a Coin",choices = checkbox_list),
                selectInput("selectTimeframePred", "Select a Timeframe",choices = list("1 Hour" = "1hour",
                                                                                       "4 Hour" = "4hour",
                                                                                       "8 Hour" = "8hour",
                                                                                       "1 Day" = "1day")),
                actionButton("predictButtonPred","Predict", icon = icon('chart-simple'), class = "btn-primary", style='padding:4px; width:100%')
                
            ),
            box(title = "Prediction", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                valueBoxOutput("Prediction", width = 12),
                textOutput("predictionText"),
                textOutput("warningText")
              
            )
          ),

          column(width = 6,
            box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                plotlyOutput('candlestickPlotPred')
            )
          ),
          img(src='fulllogo-removebg3.png', width = 200, height = 200, align = 'right' )
          

        )
      ),
      
      tabItem(tabName = "create",
              fluidRow(
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                setBackgroundImage(
                  # color = "black",
                  src = "tower.jpg",
                  shinydashboard = TRUE
                ),
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
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
                           actionButton("predictButton","Predict", icon = icon('chart-simple'), class = "btn-primary", style='padding:4px; width:100%')
                       ),
                       box(title = "Predicted High, Low, Close", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                           
                           br(),
                           br(),
                           valueBoxOutput("High", width = 12),
                           valueBoxOutput("Low", width = 12),
                           valueBoxOutput("Close", width = 12)
                       ),
                       box(title = "Predicted Break High/Low", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                           valueBoxOutput("BreakHigh", width = 12),
                           valueBoxOutput("BreakLow", width = 12)
                           
                       ),
                       box(title = "Predicted 1% Increase", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                           valueBoxOutput("oneperc", width = 12)
                       )
                ),
                
                column(width=6,
                       box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                           plotlyOutput('candlestickPlot')
                       )
                ),
                img(src='fulllogo-removebg3.png', width = 200, height = 200, align = 'right' )
              )
      ),
      tabItem(tabName = 'backtest',
              fluidRow(
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                
                column(width = 6,
                box(title = "Back-Test Inputs", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
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
                    actionButton("backTestButton","Predict", icon = icon('chart-simple'), class = "btn-primary", style='padding:4px; width:100%')
                    
                    ),
                box(title = "Histogram", solidHeader = TRUE, status = "danger", width = NULL,background = "black",
                    plotOutput("histogram")
                )

                ),

                box(title = "Back-Test Metrics", solidHeader = TRUE, status = "danger",background = "black",
                    valueBoxOutput("precision", width = 12),
                    valueBoxOutput("recall", width = 12),
                    valueBoxOutput("f1", width = 12),
                    
                    valueBoxOutput("rmse", width = 12),
                    valueBoxOutput("current.price", width = 12)
                    
                ),
                img(src='fulllogo-removebg3.png', width = 200, height = 200, align = 'right' )
              )

              
        
      ),
      tabItem(tabName = "info",
              box(title = "Method Used", solidHeader = TRUE, status = "danger",background = "black",
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
              box(title = "Future Additions", solidHeader = TRUE, status = "danger",background = "black",
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
                  
              ),
              img(src='fulllogo-removebg3.png', width = 200, height = 200, align = 'right' )
              
              
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
    
    
    output$High = renderValueBox({
      valueBox(subtitle = "Predicted High",value = text.high, icon = icon("arrow-trend-up"), color = "green")
    })
    output$Low = renderValueBox({
      valueBox(subtitle = "Predicted Low",value = text.low, icon = icon("arrow-trend-up"), color = "red")
    })
    output$Close = renderValueBox({
      valueBox(subtitle = "Predicted Close",value = text.close, icon = icon("arrow-trend-up"), color = "blue")
    })
    
    output$BreakHigh = renderValueBox({
      valueBox(subtitle = "Confidence to Break Previous High",value = text.bh, icon = icon("arrow-trend-up"), color = "green")
    })
    output$BreakLow = renderValueBox({
      valueBox(subtitle = "Confidence to Break Previous Low",value = text.bl, icon = icon("arrow-trend-up"), color = "red")
    })
    
    output$oneperc = renderValueBox({
      valueBox(subtitle = "Confidence to Break 1% Increase",value = text.perc1, icon = icon("arrow-trend-up"), color = "green")
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
    
    output$precision = renderValueBox({
      valueBox(subtitle = "Precision",value = precision, icon = icon("arrow-trend-up"), color = "red")
    })
    output$recall = renderValueBox({
      valueBox(subtitle = "recall",value = recall, icon = icon("arrow-trend-up"), color = "red")
    })
    output$f1 = renderValueBox({
      valueBox(subtitle = "F1 Score",value = f1, icon = icon("arrow-trend-up"), color = "red")
    })
    output$rmse = renderValueBox({
      valueBox(subtitle = "RMSE",value = rmse, icon = icon("arrow-trend-up"), color = "red")
    })
    output$current.price = renderValueBox({
      valueBox(subtitle = "Current Price",value = current.price, icon = icon("arrow-trend-up"), color = "red")
    })
    output$histogram = renderPlot(p1)
    
  })
  
  observeEvent(input$predictButtonPred,{
    predict.hlc(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.blbh(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.target(input$selectCandlePred, input$selectTimeframePred, "no detail")
    MakePrediction(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc)
    
    if(pred.count <= 0){
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Do Not Buy",value = pred.count, icon = icon("arrow-trend-up"), color = "red")
      })
      output$predictionText = renderText("The models are showing that buying during this candle would be a poor choice.")
    }else if(pred.count == 1){
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Unclear Decision", value = pred.count, icon = icon("arrow-trend-up"), color = "yellow")
      })
      output$predictionText = renderText("The models are giving mixed signals on if buying during this candle would be profitable.")
    }else{
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Buy Signal",value = pred.count, icon = icon("arrow-trend-up"), color = "green")
      })
      output$predictionText = renderText("The models are showing that buying at the beginning of this candle may prove proffitable.")
    }
    # output$warningText = renderText("Please note that these predictions should be used in confluence with other indicators.")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
