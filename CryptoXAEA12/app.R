library(shiny)
library(shinydashboard)
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
library(httr)
library(sass)
library(waiter)
library(shinyCopy2clipboard)
library(lubridate)
library(chron)
library(fresh)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#434C5E",
    dark_hover_bg = "#81A1C1",
    dark_color = "#FFF"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

# css <- sass(sass_file("www/chat.scss"))
# jscode <- 'var container = document.getElementById("chat-container");
# if (container) {
#   var elements = container.getElementsByClassName("user-message");
#   if (elements.length > 1) {
#     var lastElement = elements[elements.length - 1];
#     lastElement.scrollIntoView({
#       behavior: "smooth"
#     });
#   }
# }'
# 
# chatGPT_R <- function(apiKey, prompt, model="gpt-3.5-turbo") {
#   response <- POST(
#     url = "https://api.openai.com/v1/chat/completions",
#     add_headers(Authorization = paste("Bearer", apiKey)),
#     content_type("application/json"),
#     encode = "json",
#     body = list(
#       model = model,
#       messages = list(
#         list(role = "user", content = prompt)
#       )
#     )
#   )
#   
#   if(status_code(response)>200) {
#     result <- trimws(content(response)$error$message)
#   } else {
#     result <- trimws(content(response)$choices[[1]]$message$content)
#   }
#   
#   return(result)
#   
# }
# 
# execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
#   observeEvent(once = TRUE, reactiveValuesToList(session$input), {
#     force(expr)
#   }, ignoreInit = TRUE)
# }

riingo_set_token("6fbd6ce7c9e035489f6238bfab127fcedbe34ac2")

Sys.setenv(TZ="UTC")
source("Funcs.R")

str1 = readRDS("tickers/str1.rds")

checkbox_list = setNames(str1, str1)


# Define UI
ui <- dashboardPage(
  # skin = "blue",

  # dashboardHeader(
  #   title = "AI XAEA-12"
  # ),
  title = "AI XAEA-12",
  dashboardHeader(title = tags$a(tags$text("AI XAEA-12"),
                                 tags$img(src="logo.nobg.png",height="40",width="40"),
                                 style="color: white")),
  # dashboardHeader(title = shinyDashboardLogo(
  #   theme = "poor_mans_flatly",
  #   boldText = "Crypto Predictor",
  #   mainText = img(src='fulllogo-removebg.png', width = 70, height = 70),
  #   badgeText = "v1.0"
  # ),
  # titleWidth = 300
  # ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Predict", tabName = "predict", icon = icon("robot")),
      menuItem(text = "Additional Model Info", tabName = "create", icon = icon("magnifying-glass-chart")),
      menuItem(text = "Back-Test", tabName = "backtest", icon = icon("vial-circle-check")),
      menuItem(text = "General Info", tabName = "info", icon = icon("circle-info"))
      # menuItem(text = "ChatGPT", tabName = "cgpt", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    use_theme(mytheme),
    shinyjs::useShinyjs(),
    useWaiter(),
    use_copy(),
    tabItems(
      tabItem(tabName = "predict",
        fluidPage(
          # theme = shinytheme("united"),
          add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
          column(width = 6,
            box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                selectInput("selectCandlePred", "Select a Coin",choices = checkbox_list),
                selectInput("selectTimeframePred", "Select a Timeframe",choices = list("15 Minutes" = "15min",
                                                                                       "1 Hour" = "1hour",
                                                                                       "2 Hour" = "2hour",
                                                                                       "4 Hour" = "4hour",
                                                                                       "8 Hour" = "8hour",
                                                                                       "1 Day" = "1day")),
                sliderInput("selectTargetPercentage","Select a Target Increase %", min = 0.1, max = 1, step = 0.1, value = 1),
                actionButton("predictButtonPred","Predict", icon = icon('chart-simple'), style='padding:4px; width:100%'),
                br(),
                br(),
                textOutput("timer")
                
            ),
            box(title = "Prediction", solidHeader = TRUE, status = "primary", width = NULL,
                valueBoxOutput("Prediction", width = 12),
                br(),
                br(),
                textOutput("noteText"),
                textOutput("predictionText"),
                textOutput("warningText"),
                textOutput("infoText")
                
              
            )
          ),

          column(width = 6,
            box(title = "Live Candle Chart", solidHeader = TRUE, status = "primary", width = NULL,
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
                  # color = "white",
                  src = "grey-umb.jpg",
                  shinydashboard = TRUE
                ),
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                           selectInput("selectCandle", "Select a Coin",choices = checkbox_list),
                           selectInput("selectTimeframe", "Select a Timeframe",choices = list("15 Minutes" = "15min",
                                                                                              "1 Hour" = "1hour",
                                                                                              "2 Hour" = "2hour",
                                                                                              "4 Hour" = "4hour",
                                                                                              "8 Hour" = "8hour",
                                                                                              "1 Day" = "1day")),
                           # actionBttn("predictButton",
                           #            "Predict",
                           #            icon = icon('chart-simple'),
                           #            style = "jelly",
                           #            color = "primary",
                           #            block = TRUE),
                           actionButton("predictButton","Predict", icon = icon('chart-simple'), style='padding:4px; width:100%')
                       ),
                       box(title = "Predicted High, Low, Close", solidHeader = TRUE, status = "primary", width = NULL,
                           
                           br(),
                           br(),
                           valueBoxOutput("High", width = 12),
                           valueBoxOutput("Low", width = 12),
                           valueBoxOutput("Close", width = 12)
                       ),
                       box(title = "Predicted Break High/Low", solidHeader = TRUE, status = "primary", width = NULL,
                           valueBoxOutput("BreakHigh", width = 12),
                           valueBoxOutput("BreakLow", width = 12)
                           
                       ),
                       box(title = "Predicted 1% Increase", solidHeader = TRUE, status = "primary", width = NULL,
                           valueBoxOutput("oneperc", width = 12)
                       )
                ),
                
                column(width=6,
                       box(title = "Live Candle Chart", solidHeader = TRUE, status = "primary", width = NULL,
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
                box(title = "Back-Test Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                    selectInput("selectBack", "Select a Coin",choices = checkbox_list),
                    selectInput("selectTimeframeBack", "Select a Timeframe",choices = list("15 Minutes" = "15min",
                                                                                           "1 Hour" = "1hour",
                                                                                           "2 Hour" = "2hour",
                                                                                           "4 Hour" = "4hour",
                                                                                           "8 Hour" = "8hour",
                                                                                           "1 Day" = "1day")),
                    selectInput("selectPredictBack", "Select Prediction",choices = list("Break High" = "BreakH",
                                                                                           "Break Low" = "BreakL",
                                                                                           "Break 1%" = "Break1",
                                                                                           "High" = "High",
                                                                                           "Low" = "Low",
                                                                                           "Close" = "Close")),
                    actionButton("backTestButton","Predict", icon = icon('chart-simple'), style='padding:4px; width:100%')
                    
                    ),
                box(title = "Histogram", solidHeader = TRUE, status = "primary", width = NULL,
                    plotOutput("histogram")
                )

                ),

                box(title = "Back-Test Metrics", solidHeader = TRUE, status = "primary",
                    textOutput("precisionText"),
                    br(),
                    valueBoxOutput("precision", width = 12),
                    textOutput("recallText"),
                    br(),
                    valueBoxOutput("recall", width = 12),
                    textOutput("f1Text"),
                    br(),
                    valueBoxOutput("f1", width = 12),
                    
                    valueBoxOutput("rmse", width = 12),
                    valueBoxOutput("current.price", width = 12)
                    
                ),
                img(src='fulllogo-removebg3.png', width = 200, height = 200, align = 'right' )
              )

              
        
      ),
      tabItem(tabName = "info",
              box(title = "Method Used", solidHeader = TRUE, status = "primary",
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
              box(title = "Future Additions", solidHeader = TRUE, status = "primary",
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
      
      # tabItem(tabName = "cgpt",
      #   fluidPage(
      #     tags$head(tags$style(css)),
      #     sidebarLayout(
      #       sidebarPanel(
      #         textInput("apiKey", "API Key", "sk-xxxxxxxxxxxxxxxxxxxx"),
      #         selectInput("model", "Model", choices = c("gpt-3.5-turbo", "gpt-4"), selected = "gpt-3.5-turbo"),
      #         style = "background-color: #fff; color: #333; border: 1px solid #ccc;"
      #       ),
      #       
      #       mainPanel(
      #         tags$div(
      #           id = "chat-container",
      #           tags$div(
      #             id = "chat-header",
      #             tags$img(src = "TnUa864.png", alt = "AI Profile Picture"),
      #             tags$h3("AI Assistant")
      #           ),
      #           tags$div(
      #             id = "chat-history",
      #             uiOutput("chatThread"),
      #           ),
      #           tags$div(
      #             id = "chat-input",
      #             tags$form(
      #               column(12,textAreaInput(inputId = "prompt", label="", placeholder = "Type your message here...", width = "100%")),
      #               fluidRow(
      #                 tags$div(style = "margin-left: 1.5em;",
      #                          actionButton(inputId = "submit",
      #                                       label = "Send",
      #                                       icon = icon("paper-plane")),
      #                          actionButton(inputId = "remove_chatThread",
      #                                       label = "Clear History",
      #                                       icon = icon("trash-can")),
      #                          CopyButton("clipbtn",
      #                                     label = "Copy",
      #                                     icon = icon("clipboard"),
      #                                     text = "")
      #                          
      #                 ))
      #             ))
      #         )
      #       ))
      #     
      #   )
      # )
    )
  )
  
  
)

# Define server logic
server <- function(input, output, session) {
  
  dateTime = reactiveVal(Sys.time())
  
  output$timer = renderText(paste0("Time reamining in this candle: ",dateTime()))

  historyALL <- reactiveValues(df = data.frame() , val = character(0))
  
  # On click of send button
  observeEvent(input$submit, {
    
    if (nchar(trimws(input$prompt)) > 0) {
      
      # Spinner
      w <- Waiter$new(id = "chat-history",
                      html = spin_3(),
                      color = transparent(.5))
      w$show()
      
      # Response
      chatGPT <- chatGPT_R(input$apiKey, input$prompt, input$model)
      historyALL$val <- chatGPT
      history <- data.frame(users = c("Human", "AI"),
                            content = c(input$prompt, chatGPT),
                            stringsAsFactors = FALSE)
      historyALL$df <- rbind(historyALL$df, history)
      updateTextInput(session, "prompt", value = "")
      
      # Conversation Interface
      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          tags$div(class = ifelse(historyALL$df[x, "users"] == "Human",
                                  "user-message",
                                  "bot-message"),
                   HTML(paste0(ifelse(historyALL$df[x, "users"] == "Human",
                                      "
<img src='girl.avif' class='img-wrapper2'>
",
                                      "
<img src='boy.avif' class='img-wrapper2'>
"),
                               historyALL$df[x, "content"])))
        })
        do.call(tagList, conversations)
      })
      
      w$hide()
      execute_at_next_input(runjs(jscode))
      
    }
    
  })
  
  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({return(NULL)})
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })
  
  observe({
    invalidateLater(1000, session)
    isolate({
      dateTime(getTimeRemaining(input$selectTimeframePred))
      # dateTime(dateTime()-1)
    })
    
    req(input$clipbtn)
    CopyButtonUpdate(session,
                     id = "clipbtn",
                     label = "Copy",
                     icon = icon("clipboard"),
                     text = as.character(historyALL$val))
  })
  
  
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
    predict.target(input$selectCandle, input$selectTimeframe, "detail", input$selectTargetPercentage)
    
    
    output$High = renderValueBox({
      valueBox(subtitle = "Predicted High",value = text.high, icon = icon("arrow-trend-up"), color = "green")
    })
    output$Low = renderValueBox({
      valueBox(subtitle = "Predicted Low",value = text.low, icon = icon("arrow-trend-up"), color = "red")
    })
    output$Close = renderValueBox({
      valueBox(subtitle = "Predicted Close",value = text.close, icon = icon("arrow-trend-up"), color = "orange")
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
    
    output$precisionText = renderText("- The precision metric is a measure of how accurate the model is when classifying a prediction as positive. The equation goes as: (true positives) / (true positives + false positives)")
    output$recallText = renderText("- The recall metric is a measure of what percentage of positives WERE classified as positive compared to how many SHOULD have been classified as positive. The equation goes as: (true positives) / (true positives + false negatives)")
    output$f1Text = renderText("- The f1 score is a value from 0 to 1, 1 being the model classifies every observation correctly. The equation goes as: 2 * (precision * recall) / (precision + recall)")
    
    
    if(input$selectPredictBack == "High" |input$selectPredictBack == "Low" |input$selectPredictBack == "Close"){
      hide("precision")
      hide("recall")
      hide("f1")
      hide("precisionText")
      hide("recallText")
      hide("f1Text")
      shinyjs::show("rmse")
      shinyjs::show("current.price")
      hide("histogram")
    }else{
      shinyjs::show("precision")
      shinyjs::show("recall")
      shinyjs::show("f1")
      shinyjs::show("precisionText")
      shinyjs::show("recallText")
      shinyjs::show("f1Text")
      shinyjs::show("histogram")
      hide("rmse")
      hide("current.price")
    }
    
    output$precision = renderValueBox({
      valueBox(subtitle = "Precision",value = precision, icon = icon("check"), color = "green")
    })
    output$recall = renderValueBox({
      valueBox(subtitle = "Recall",value = recall, icon = icon("check"), color = "green")
    })
    output$f1 = renderValueBox({
      valueBox(subtitle = "F1 Score",value = f1, icon = icon("check"), color = "green")
    })
    output$rmse = renderValueBox({
      valueBox(subtitle = "RMSE",value = rmse, icon = icon("check"), color = "green")
    })
    output$current.price = renderValueBox({
      valueBox(subtitle = "Current Price",value = current.price, icon = icon("check"), color = "green")
    })
    output$histogram = renderPlot(p1)
    
  })
  
  observeEvent(input$predictButtonPred,{
    predict.hlc(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.blbh(input$selectCandlePred, input$selectTimeframePred, "no detail")
    predict.target(input$selectCandlePred, input$selectTimeframePred, "no detail", input$selectTargetPercentage)
    MakePrediction(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc)
    
    if(pred.count < 0){
      output$Prediction = renderValueBox({
        valueBox(value = "Weak Signal",subtitle = "- The models are showing that buying during this candle may be a poor choice.", icon = icon("arrow-trend-up"), color = "red")
      })
      # output$predictionText = renderText("- The models are showing that buying during this candle would be a poor choice.")
    }else if(pred.count == 0 | pred.count == 1){
      output$Prediction = renderValueBox({
        valueBox(value = "Unclear Decision", subtitle = "- The models are giving mixed signals on if buying during this candle would be profitable.", icon = icon("arrow-trend-up"), color = "yellow")
      })
      # output$predictionText = renderText("- The models are giving mixed signals on if buying during this candle would be profitable.")
    }else{
      output$Prediction = renderValueBox({
        valueBox(value = "Buy Signal",subtitle = "- The models are showing that buying at the beginning of this candle may prove proffitable.", icon = icon("arrow-trend-up"), color = "green")
      })
      # output$predictionText = renderText("- The models are showing that buying at the beginning of this candle may prove proffitable.")
    }
    output$warningText = renderText("- Please note that these predictions should be used in confluence with other indicators. There is no magic indicator that is always right.")
    output$infoText = renderText("- For more info on the models, see the 'General Info' tab!")
    # output$noteText = renderText("- Note that the predictions range from -4 to 4. -4 for being a strong DON'T BUY signal, and 4 being a strong BUY signal.")
  })
  
  observeEvent(input$selectTimeframePred, {
    if(input$selectTimeframePred == "15min" | input$selectTimeframePred == "1hour"){
      updateSliderInput(inputId = "selectTargetPercentage", label = "Select a Target Increase %", min = 0.1, max = 1, step = 0.1, value = 1)
    }else if(input$selectTimeframePred == "2hour"){
      updateSliderInput(inputId = "selectTargetPercentage", label = "Select a Target Increase %", min = 0.1, max = 2, step = 0.1, value = 1)
    }else{
      updateSliderInput(inputId = "selectTargetPercentage", label = "Select a Target Increase %", min = 0.2, max = 3, step = 0.2, value = 1)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
