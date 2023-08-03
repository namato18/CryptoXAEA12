Sys.setenv(TZ="UTC")

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

LivePlot = function(symbol, timeframe){
  # symbol = 'BTCUSDT'
  # timeframe = "1day"
  
  
  ################################## CHECK BACK IN ON THIS ######################################3
  if(timeframe != "1day"){
    df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 21, end_date = Sys.Date(), resample_frequency = timeframe)
    df1 = df1[-nrow(df1),]
    df2 = riingo_crypto_latest(symbol, resample_frequency = timeframe)
    
    df = rbind(df1,df2)
  }else{
    df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 21, end_date = Sys.Date(), resample_frequency = timeframe)
    df2 = riingo_crypto_latest(symbol, resample_frequency = timeframe)
    
    df = rbind(df1,df2)
  }

  df = df[,-c(1:3,10:11)]
  
  df$date = str_replace(string = df$date, pattern = "T", replacement = " ")
  df$date = str_replace(string = df$date, pattern = "Z", replacement = "")
  if(timeframe =="1day"){
    df$date = as.POSIXct(df$date, format = "%Y-%m-%d")
    
  }else{
    df$date = as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
  }

  df_candle_plot = tail(df,30) %>%
    plot_ly(x = ~date, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low)
  df_candle_plot = df_candle_plot %>% layout(title = paste0('Last 30 candles for ',toupper(symbol)," ",timeframe),
                                             xaxis = list(rangeslider = list(visible = F)))
  return(df_candle_plot)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.hlc = function(symbol, timeframe, output){
  # symbol = 'BTCUSDT'
  # timeframe = "1day"
  
  df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
  df1 = df1[-nrow(df1),]
  df2 = riingo_crypto_latest(symbol, resample_frequency = timeframe)
  
  df = rbind(df1,df2)
  df = df[,-c(1:3,10:11)]
  
  df$date = str_replace(string = df$date, pattern = "T", replacement = " ")
  df$date = str_replace(string = df$date, pattern = "Z", replacement = "")
  if(timeframe =="1day"){
    df$date = as.POSIXct(df$date, format = "%Y-%m-%d")
    
  }else{
    df$date = as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
  }
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close","Volume")
  
  
  ###############################
  ############################### ADD IN MOVING AVERAGES
  df$MA10 = NA
  df$MA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    df$MA20[k] = mean(df$Close[k-20:k])
  }
  
  ###############################
  ############################### DEFINE OTHER INPUT VALUES
  df$OH = (df$High - df$Open)/df$Open * 100
  df$CH = (df$Close - df$Open)/ df$Open * 100
  df$LH = (df$High - df$Low) / df$Low * 100
  df$LC = (df$Close - df$Low) / df$Low * 100
  
  df$HMA = (df$High - df$MA20)/ df$MA20 * 100
  df$LMA = (df$Low - df$MA20)/ df$MA20 * 100
  df$CMA = (df$Close - df$MA20)/ df$MA20 * 100
  
  
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[-c(1:20,nrow(df)),-c(1)]
  
  ###############################
  ############################### GRAB SECOND TO LAST ROW FOR PREDICTION AND TURN TO MATRIX
  
  df = df[nrow(df)-1,]
  df.m = as.matrix(df)
  
  ###############################
  ############################### GRAB BOOSTED MODEL
  bst.open = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Open.rds"))
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Close.rds"))
    
  ###############################
  ############################### PREDICT
  
  pred.open = predict(bst.open, df.m)
  pred.high = predict(bst.high, df.m)
  pred.low = predict(bst.low, df.m)
  pred.close = predict(bst.close, df.m)
  
  if(pred.close >= pred.high){
    pred.high = pred.close
  }
  if(pred.close <= pred.low){
    pred.low = pred.close
  }
  
  ###############################
  ############################### CALCULATE PERCENT CHANGES
  
  perc.high = round((pred.high - df$Close) / df$Close * 100, digits = 2)
  perc.low = round((pred.low - df$Close) / df$Close * 100, digits = 2)
  perc.close = round((pred.close - df$Close) / df$Close * 100, digits = 2)

  
  output$High = renderInfoBox({
    infoBox("Predicted High", paste0(round(pred.high, digits = 3), "(",perc.high,"%)"),icon = icon("arrow-trend-up"), color = "red")
  })
  output$Low = renderInfoBox({
    infoBox("Predicted Low", paste0(round(pred.low, digits = 3), "(",perc.low,"%)"),icon = icon("arrow-trend-down"), color = "red")
  })
  output$Close = renderInfoBox({
    infoBox("Predicted Close", paste0(round(pred.close, digits = 3), "(",perc.close,"%)"),icon = icon("flag-checkered"), color = "red")
  })

  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.blbh = function(symbol, timeframe, output){
  symbol = "BTCUSDT"
  timeframe = "1hour"
  
  df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
  df1 = df1[-nrow(df1),]
  df2 = riingo_crypto_latest(symbol, resample_frequency = timeframe)
  
  df = rbind(df1,df2)
  df = df[,-c(1:3,10:11)]
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close","Volume")
  
  
  ###############################
  ############################### ADD IN MOVING AVERAGES
  df$MA10 = NA
  df$MA20 = NA
  df$VMA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    df$MA20[k] = mean(df$Close[k-20:k])
    df$VMA20[k]= mean(df$Volume[k-20:k])
  }
  
  ###############################
  ############################### ADD IN CHECKS FOR CLOSING VALUES
  C1 = rep(0, nrow(df))
  C2 = rep(0, nrow(df))
  C3 = rep(0, nrow(df))
  
  for(k in 4:nrow(df)){
    if(df$Close[k] > df$Close[k-1]){
      C1[k] = 1
    }
    if(df$Close[k-1] > df$Close[k-2]){
      C2[k] = 1
    }
    if(df$Close[k-2] > df$Close[k-3]){
      C3[k] = 1
    }
  }
  
  df$P3C = C1 + C2 + C3
  
  
  ###############################
  ############################### DEFINE OTHER INPUT VALUES
  df$OH = (df$High - df$Open)/df$High * 100
  df$CH = (df$Close - df$Open)/ df$Close * 100
  df$LH = (df$High - df$Low) / df$High * 100
  df$LC = (df$Close - df$Low) / df$Low * 100
  
  df$HMA = (df$High - df$MA20)/ df$High * 100
  df$LMA = (df$Low - df$MA20)/ df$Low * 100
  df$CMA = (df$Close - df$MA20)/ df$Close * 100
  df$VMA = (df$Volume - df$VMA20) / df$Volume * 100
  
  lag1Vol = Lag(df$Volume, 1)
  df$VolumeD = (df$Volume - lag1Vol)/df$Volume * 100
  
  ###############################
  ############################### DETERMINE OUTCOME VALUES
  BreakL = NA
  BreakH = NA
  
  for(k in 2:(nrow(df))){
    if(df$Low[k] <= df$Low[k-1]){
      BreakL[k] = 1
    }else{
      BreakL[k] = 0
    }
    
    if(df$High[k] >= df$High[k-1]){
      BreakH[k] = 1
    }else{
      BreakH[k] = 0
    }
  }
  
  BreakH = c(BreakH, NA)
  BreakH = BreakH[-1]
  
  BreakL = c(BreakL, NA)
  BreakL = BreakL[-1]
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[-c(1:20,nrow(df)),-c(1:5)]
  BreakL = BreakL[-c(1:20,length(BreakL))]
  BreakH = BreakH[-c(1:20,length(BreakH))]
  
  
  ###############################
  ############################### ROUND ALL INPUTS TO 2 DIGITS
  df = round(df, 2)
  
  ###############################
  ############################### SELECT ONLY CERTAIN INPUTS FOR BUILDING THE MODEL
  df = df %>%
    select("LH","LC","VolumeD","VMA","LMA","HMA","P3C")
  
  df.m = as.matrix(df)
  
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"BreakH.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"BreakL.rds"))
  
}

