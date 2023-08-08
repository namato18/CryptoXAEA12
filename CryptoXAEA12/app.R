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
library(httr)
library(sass)
library(waiter)
library(shinyCopy2clipboard)

css <- sass(sass_file("www/chat.scss"))
jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'

chatGPT_R <- function(apiKey, prompt, model="gpt-3.5-turbo") {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type("application/json"),
    encode = "json",
    body = list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      )
    )
  )
  
  if(status_code(response)>200) {
    result <- trimws(content(response)$error$message)
  } else {
    result <- trimws(content(response)$choices[[1]]$message$content)
  }
  
  return(result)
  
}

execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
  observeEvent(once = TRUE, reactiveValuesToList(session$input), {
    force(expr)
  }, ignoreInit = TRUE)
}

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
      # menuItem(text = "ChatGPT", tabName = "cgpt", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    useWaiter(),
    use_copy(),
    tabItems(
      tabItem(tabName = "predict",
        fluidPage(
          # theme = shinytheme("cyborg"),
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
                br(),
                br(),
                textOutput("noteText"),
                textOutput("predictionText"),
                textOutput("warningText"),
                textOutput("infoText")
                
              
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
                  # color = "white",
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
              
              
      ),
      
      tabItem(tabName = "cgpt",
        fluidPage(
          tags$head(tags$style(css)),
          sidebarLayout(
            sidebarPanel(
              textInput("apiKey", "API Key", "sk-xxxxxxxxxxxxxxxxxxxx"),
              selectInput("model", "Model", choices = c("gpt-3.5-turbo", "gpt-4"), selected = "gpt-3.5-turbo"),
              style = "background-color: #fff; color: #333; border: 1px solid #ccc;"
            ),
            
            mainPanel(
              tags$div(
                id = "chat-container",
                tags$div(
                  id = "chat-header",
                  tags$img(src = "TnUa864.png", alt = "AI Profile Picture"),
                  tags$h3("AI Assistant")
                ),
                tags$div(
                  id = "chat-history",
                  uiOutput("chatThread"),
                ),
                tags$div(
                  id = "chat-input",
                  tags$form(
                    column(12,textAreaInput(inputId = "prompt", label="", placeholder = "Type your message here...", width = "100%")),
                    fluidRow(
                      tags$div(style = "margin-left: 1.5em;",
                               actionButton(inputId = "submit",
                                            label = "Send",
                                            icon = icon("paper-plane")),
                               actionButton(inputId = "remove_chatThread",
                                            label = "Clear History",
                                            icon = icon("trash-can")),
                               CopyButton("clipbtn",
                                          label = "Copy",
                                          icon = icon("clipboard"),
                                          text = "")
                               
                      ))
                  ))
              )
            ))
          
        )
      )
    )
  )
  
  
)

# Define server logic
server <- function(input, output, session) {
  
  
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
    predict.target(input$selectCandle, input$selectTimeframe, "detail")
    
    
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
      valueBox(subtitle = "Precision",value = precision, icon = icon("check"), color = "green")
    })
    output$recall = renderValueBox({
      valueBox(subtitle = "recall",value = recall, icon = icon("check"), color = "green")
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
    predict.target(input$selectCandlePred, input$selectTimeframePred, "no detail")
    MakePrediction(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc)
    
    if(pred.count < 0){
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Do Not Buy",value = pred.count, icon = icon("arrow-trend-up"), color = "red")
      })
      output$predictionText = renderText("- The models are showing that buying during this candle would be a poor choice.")
    }else if(pred.count == 0 | pred.count == 1){
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Unclear Decision", value = pred.count, icon = icon("arrow-trend-up"), color = "yellow")
      })
      output$predictionText = renderText("- The models are giving mixed signals on if buying during this candle would be profitable.")
    }else{
      output$Prediction = renderValueBox({
        valueBox(subtitle = "Buy Signal",value = pred.count, icon = icon("arrow-trend-up"), color = "green")
      })
      output$predictionText = renderText("- The models are showing that buying at the beginning of this candle may prove proffitable.")
    }
    output$warningText = renderText("- Please note that these predictions should be used in confluence with other indicators.")
    output$infoText = renderText("- For more info on the models, see the 'General Info' tab!")
    output$noteText = renderText("- Note that the predictions range from -4 to 4. -4 for being a strong DON'T BUY signal, and 4 being a strong BUY signal.")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
