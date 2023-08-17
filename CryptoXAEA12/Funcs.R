Sys.setenv(TZ="UTC")

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################


# CALC TIME TO NEXT CANDLE CLOSE
getTimeRemaining = function(timeframe){
  
  utcTime = lubridate::now(tzone = 'UTC')
  utcTime = format(utcTime, format = "%H:%M:%S")
  
  if(timeframe == "15min"){
    hour.t = hours(chron(times=utcTime))
    minutes.t = minutes(chron(times=utcTime))
    
    candle.times = floor(seq(from = 0, to = 60, by = 14.95))
    
    
    ind = which(candle.times >= minutes.t)[1]
    candle.t.ind = candle.times[ind]
    
    end.of.candle = chron(times=paste0(hour.t,":",candle.t.ind,":59"))
    
    remainingTime = end.of.candle - utcTime
    return(remainingTime)
  }
  
  if(timeframe == "1hour"){
    hour.t = hours(chron(times=utcTime))
    
    end.of.candle = chron(times=paste0(hour.t,":59:59"))
    
    remainingTime = end.of.candle - utcTime
    return(remainingTime)
  }
  
  if(timeframe == "2hour"){
    hour.t = hours(chron(times=utcTime))
    
    two.hrs = seq(from = 0, to = 24, by = 2)
    
    if(any(hour.t %in% two.hrs)){
      end.of.candle = chron(times=paste0(as.numeric(hour.t)+1,":59:59"))
    }else{
      end.of.candle = chron(times=paste0(hour.t,":59:59"))
    }
    remainingTime = end.of.candle - utcTime
    
    return(remainingTime)
  }
  
  if(timeframe == '4hour'){
    if(utcTime >= chron(times="20:00:00")){
      remainingTime = chron(times="23:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="16:00:00")){
      remainingTime = chron(times="19:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="12:00:00")){
      remainingTime = chron(times="15:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="08:00:00")){
      remainingTime = chron(times="11:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="04:00:00")){
      remainingTime = chron(times="7:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="00:00:00")){
      remainingTime = chron(times="3:59:59") - utcTime
      return(remainingTime)
    }
  }
  if(timeframe == '8hour'){
    if(utcTime >= chron(times="16:00:00")){
      remainingTime = chron(times="23:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="08:00:00")){
      remainingTime = chron(times="15:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="00:00:00")){
      remainingTime = chron(times="7:59:59") - utcTime
      return(remainingTime)
    }
  }
  if(timeframe == '1day'){
    remainingTime = chron(times="23:59:59") - utcTime
    return(remainingTime)
    
  }
  
}

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

predict.hlc = function(symbol, timeframe, tab){
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
  
  if(tab == "detail"){
    assign("text.high",text.high,.GlobalEnv)
    assign("text.low",text.low,.GlobalEnv)
    assign("text.close",text.close,.GlobalEnv)
  }else{
    assign("perc.high",perc.high,.GlobalEnv)
    assign("perc.low",perc.low,.GlobalEnv)
    assign("perc.close",perc.close,.GlobalEnv)
  }

  
  

  # return(paste0("$",round(pred, digits = 5), "(",perc,"%)"))
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.blbh = function(symbol, timeframe, tab){
  # symbol = "BTCUSDT"
  # timeframe = "1hour"
  # tab = "no detail"
  
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
  
  if(tab == "detail"){
    assign('text.bh',text.bh,.GlobalEnv)
    assign('text.bl',text.bl,.GlobalEnv)
  }else{
    assign('pred.bh',pred.bh,.GlobalEnv)
    assign('pred.bl',pred.bl,.GlobalEnv)
    assign("prev.low.perc",prev.low.perc,.GlobalEnv)
    assign("prev.high.perc",prev.high.perc,.GlobalEnv)
    
  }

  
  
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

predict.target = function(symbol, timeframe,tab, target.percentage){
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

  down.trend.df = data.frame(down.trend(df))
  up.trend.df = data.frame(up.trend(df))
  down.trend.df = down.trend.df$Down.Trend[nrow(down.trend.df)]
  up.trend.df = up.trend.df$Up.Trend[nrow(up.trend.df)]
  
  assign("up.trend",up.trend.df,.GlobalEnv)
  assign("down.trend",down.trend.df,.GlobalEnv)
  
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
  
  bst1 = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,target.percentage,".rds"))
  
  pred = round(predict(bst1, df.m), digits = 3)
  
  if(tab == "detail"){
    assign("text.perc1",pred,.GlobalEnv)
  }else{
    assign("pred.perc1",pred,.GlobalEnv)
  }
  
  # return(paste0(pred))

}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# symbol = 'LINAUSDT'
# timeframe = "1day"
# prediction = "High"

BackTest = function(symbol, timeframe, prediction){
  
  ###############################
  ############################### GRAB ALL COMPARE DATASETS
if(prediction == "BreakH" | prediction == "BreakL" | prediction == "Break1"){
  if(prediction == "BreakH" | prediction == "BreakL"){
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("compare_",symbol,"_",timeframe,prediction,".rds"))
  }else{
    sample.split = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("sample.split_",symbol,"_",timeframe,"1.rds"))
    outcome = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("outcome_",symbol,"_",timeframe,"1.rds"))
    test = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("test_",symbol,"_",timeframe,"1.rds"))
    bst.1perc = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",symbol,"_",timeframe,"1.rds"))
    
    outcome.test = outcome[!sample.split]
    pred.1perc = predict(bst.1perc, test)
    
    compare = data.frame(cbind(outcome.test,pred.1perc))
  }
  
  df = compare
  
  colnames(df) = c("outcome.test","pred")
  
  df$decision = 0
  df$decision[df$pred >= 0.5] = 1
  
  true.pos = length(which(df$outcome.test == 1 & df$decision == 1))
  false.pos = length(which(df$outcome.test == 0 & df$decision == 1))
  false.neg = length(which(df$outcome.test == 1 & df$decision == 0))
  
  
  precision = true.pos / (true.pos + false.pos) * 100
  recall = true.pos / (true.pos + false.neg) * 100
  f1 = 2*((precision * recall)/(precision + recall))
  
  precision = round(precision, digits = 4)
  recall = round(recall, digits = 4)
  f1 = round(f1, digits = 4)
  
  p1 = ggplot(data = df, aes(x = pred)) + geom_histogram(color = "black", fill = "blue", alpha = 0.3)
  
  assign("p1",p1,.GlobalEnv)
  
  assign("precision",precision, .GlobalEnv)
  assign("recall",recall,.GlobalEnv)
  assign("f1",f1,.GlobalEnv)
  assign("rmse",NULL,.GlobalEnv)
  assign("current.price",NULL,.GlobalEnv)
  
  
  
}else{
  
  compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("compare_",symbol,"_",timeframe,"_",prediction,".rds"))
  
  compare$error.sq = (compare$pred - compare$outcome.test)^2
  rmse = round((mean(compare$error.sq))^(1/2),digits = 5)
  
  current.price = riingo::riingo_crypto_latest(symbol, resample_frequency = timeframe)
  current.price = round(current.price$close[nrow(current.price)], digits = 5)
  
  assign("precision",NULL, .GlobalEnv)
  assign("recall",NULL,.GlobalEnv)
  assign("f1",NULL,.GlobalEnv)
  assign("rmse",rmse,.GlobalEnv)
  assign("current.price",current.price,.GlobalEnv)
  
  
  
}
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

MakePrediction = function(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc){
  pred.count = 0
  
  target.percentage.adjust.good = round(pred.perc1 * 0.8, digits = 1)
  target.percentage.adjust.bad = round(pred.perc1 * 0.6, digits = 1)
  
  
  #####################
  ##################### ADD CONDITIONS FOR BAD
  if((perc.low*-1) > perc.high){
    pred.count = pred.count - 1
  }
  if(pred.perc1 < 0.5){
    pred.count = pred.count - 1
  }
  if(pred.bl > 0.5 & prev.low.perc*-1 > target.percentage.adjust.bad){
    pred.count = pred.count - 1
  }
  if(down.trend == TRUE){
    pred.count = pred.count - 1
  }
  
  #####################
  ##################### ADD CONDITIONS FOR GOOD
  if((perc.low*-1) < perc.high & perc.high > 1){
    pred.count = pred.count + 1
  }
  if(pred.perc1 > 0.5){
    pred.count = pred.count + 1
  }
  if(pred.bh > 0.5 & prev.high.perc > target.percentage.adjust.good){
    pred.count = pred.count + 1
  }
  if(up.trend == TRUE){
    pred.count = pred.count + 1
  }
  
  assign("pred.count",pred.count,.GlobalEnv)
  
}
