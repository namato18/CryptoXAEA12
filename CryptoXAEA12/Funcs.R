Sys.setenv(TZ="UTC")

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

LivePlot = function(symbol, timeframe){
  # symbol = 'BTCUSDT'
  # timeframe = "1day"
  
  df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 21, end_date = Sys.Date(), resample_frequency = timeframe)
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

predict.bhbl = function(symbol, timeframe){
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
  ############################### ADD VECTOR OUTPUT FOR NEXT HIGH VALUE
  NextOut = df[[j]]
  NextOut[length(NextOut)+1] = NA
  NextOut = NextOut[-1]
  
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
  df = as.matrix(df)
  
  ###############################
  ############################### GRAB BOOSTED MODEL
  bst.open = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Open.rds"))
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Close.rds"))
    
  ###############################
  ############################### PREDICT
  
  pred.open = predict(bst.open, df)
  pred.high = predict(bst.high, df)
  pred.low = predict(bst.low, df)
  pred.close = predict(bst.close, df)
  
}

