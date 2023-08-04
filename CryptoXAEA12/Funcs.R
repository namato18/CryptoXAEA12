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

predict.hlc = function(symbol, timeframe){
  # symbol = 'BTCUSDT'
  # timeframe = "1hour"
  # ohlc = "High"
  
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
  ############################### ADD IN ADDITIONAL LAG VALUES
  df$CloseLag1 = Lag(df$Close, 1)
  df$CloseLag2 = Lag(df$Close, 2)
  
  df$OpenLag1 = Lag(df$Open, 1)
  df$OpenLag2 = Lag(df$Open, 2)
  
  df$HighLag1 = Lag(df$High, 1)
  df$HighLag2 = Lag(df$High, 2)
  
  df$LowLag1 = Lag(df$Low, 1)
  df$LowLag2 = Lag(df$Low, 2)
  
  
  ###############################
  ############################### DEFINE OTHER INPUT VALUES
  df$OH = (df$High - df$Open)/df$Open * 100
  df$CH = (df$Close - df$Open)/ df$Open * 100
  df$LH = (df$High - df$Low) / df$Low * 100
  df$LC = (df$Close - df$Low) / df$Low * 100
  
  df$HMA = (df$High - df$MA20)/ df$MA20 * 100
  df$LMA = (df$Low - df$MA20)/ df$MA20 * 100
  df$CMA = (df$Close - df$MA20)/ df$MA20 * 100
  
  lag1Vol = Lag(df$Volume, 1)
  df$VolumeD = (df$Volume - lag1Vol)/df$Volume * 100
  
  
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[-c(1:20,nrow(df)),-c(1)]
  
  ###############################
  ############################### GRAB SECOND TO LAST ROW FOR PREDICTION AND TURN TO MATRIX
  
  df = df[nrow(df)-1,]
  df = df[,1:21]
  
  df.m = as.matrix(df)
  
  ###############################
  ############################### GRAB BOOSTED MODEL
  # bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_",ohlc,".rds"))
  
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"_Close.rds"))


    
  ###############################
  ############################### PREDICT
  
  # pred = predict(bst, df.m)
  
  pred.high = predict(bst.high, df.m)
  pred.low = predict(bst.low, df.m)
  pred.close = predict(bst.close, df.m)
  
  if(pred.high <= pred.close){
    pred.high = pred.close
  }
  if(pred.low >= pred.close){
    pred.low = pred.close
  }
  if(pred.low >= df$Close){
    pred.low = df$Close
  }
  if(pred.high <= df$Close){
    pred.high = df$Close
  }
  
  # if(ohlc == "High" & pred <= df$Close){
  #   pred = df$Close
  # }
  # if(ohlc == "High" & pred <= pred.close){
  #   pred = pred.close
  # }
  # if(ohlc == "Low" & pred >= pred.close){
  #   pred = pred.close
  # }
  
  ###############################
  ############################### CALCULATE PERCENT CHANGES
  
  # perc = round((pred - df$Close) / df$Close * 100, digits = 2)
  
  perc.high = round((pred.high - df$Close) / df$Close * 100, digits = 2)
  perc.low = round((pred.low - df$Close) / df$Close * 100, digits = 2)
  perc.close = round((pred.close - df$Close) / df$Close * 100, digits = 2)
  
  
  
  text.high = paste0("$",round(pred.high, digits = 5), "(",perc.high,"%)")
  text.low = paste0("$",round(pred.low, digits = 5), "(",perc.low,"%)")
  text.close = paste0("$",round(pred.close, digits = 5), "(",perc.close,"%)")
  
  assign("text.high",text.high,.GlobalEnv)
  assign("text.low",text.low,.GlobalEnv)
  assign("text.close",text.close,.GlobalEnv)
  
  

  # return(paste0("$",round(pred, digits = 5), "(",perc,"%)"))
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.blbh = function(symbol, timeframe){
  # symbol = "BTCUSDT"
  # timeframe = "1hour"
  # blbh = "BreakH"
  
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
  
  df = df[nrow(df)-1,]
  
  prev.high.perc = round((df$High - df$Open) / df$Open * 100, digits = 3)
  prev.low.perc = round((df$Low - df$Open) / df$Open * 100, digits = 3)
  
  ###############################
  ############################### SELECT ONLY CERTAIN INPUTS FOR BUILDING THE MODEL
  df = df %>%
    select("LH","LC","VolumeD","VMA","LMA","HMA","P3C")
  
  
  ###############################
  ############################### ROUND ALL INPUTS TO 2 DIGITS
  df = round(df, 2)
  

  
  
  df.m = as.matrix(df)
  
  # bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,blbh,".rds"))
  
  bst.bh = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"BreakH.rds"))
  bst.bl = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"BreakL.rds"))
  
  pred.bh = round(predict(bst.bh, df.m), digits = 5)
  pred.bl = round(predict(bst.bl, df.m), digits = 5)
  
  # pred = round(predict(bst, df.m), digits = 5)
  
  text.bh = paste0(pred.bh," (Previous High of ",prev.high.perc,"%)")
  text.bl = paste0(pred.bl," (Previous Low of ",prev.low.perc,"%)")
  
  assign('text.bh',text.bh,.GlobalEnv)
  assign('text.bl',text.bl,.GlobalEnv)
  
  
  # if(blbh == "BreakH"){
  #   return(paste0(pred," (Previous High of ",prev.high.perc,"%)"))
  # }else{
  #   return(paste0(pred," (Previous Low of ",prev.low.perc,"%)"))
  # }

}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.target = function(symbol, timeframe){
  # symbol = 'LINAUSDT'
  # timeframe = "1day"
  
  df1 = riingo_crypto_prices(symbol, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
  df1 = df1[-nrow(df1),]
  df2 = riingo_crypto_latest(symbol, resample_frequency = timeframe)
  
  df = rbind(df1,df2)
  df = df[,-c(1:3,10:11)]
  
  # Modify data to be more useable
  df$Percent.Change = NA
  #df = df[-1,-c(1:3,10:11)]
  colnames(df) = c("Date","Open","High","Low","Close","Volume","Percent.Change")

  df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)

  
  #Add column for binary previouos day change+
  df$Previous = NA
  for(k in 2:nrow(df)){
    if(df$Percent.Change[k - 1] <= 0){
      df$Previous[k] = 0
    }else{
      df$Previous[k] = 1
    }
  }
  
  # Remove first row since we can't use it
  df = df[-1,]
  
  # Adding Moving Averages
  df$MA10 = NA
  df$MA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    df$MA20[k] = mean(df$Close[k-20:k])
  }
  
  # Add column for if MA10 is above or below MA20
  df$MAAB = 0
  
  df$MAAB[df$MA10 > df$MA20] = 1
  
  df = df[,-which(colnames(df) %in% c("MA10","MA20"))]
  # Convert to actual dates and remove year and change to numeric
  if(!grepl(pattern ="day",timeframe)){
    df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
    df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")
    
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
  }else{
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
  }
  
  df = df[!is.na(df$Date),]
  
  df = as.xts(df)
  
  candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
  
  for(k in 1:length(candle.list)){
    df = cbind(df, candle.list[[k]])
  }

  # Remove unusable rows
  df = df[-(1:20),]
  
  # Add lagged values
  for(k in 1:5){
    high.lag = Lag(df$High, k)
    open.lag = Lag(df$Open, k)
    percent.change.lag = round((((high.lag/open.lag) - 1) * 100), digits = 2)
    df = cbind(df, percent.change.lag)
    
  }
  
  df = df[-c(1:5),]
  
  df[is.na(df)] = 0
  
  df = data.frame(df, row.names = NULL)
  
  ### Remove OPEN HIGH LOW CLOSE
  df = df[nrow(df)-1,-c(1:4)]
  df.m = as.matrix(df)
  
  bst1 = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"1.rds"))
  
  pred = round(predict(bst1, df.m), digits = 3)
  
  assign("text.perc1",pred,.GlobalEnv)
  
  # return(paste0(pred))

}
