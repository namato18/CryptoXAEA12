library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(tictoc)
library(CandleStickPattern)
library(dplyr)
library(riingo)

tic()

out.names = c("Open","High","Low","Close")

x = list.files(path = '../RiingoPulledData/',full.names = TRUE)
file.names = list.files('../RiingoPulledData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")
ls.files = lapply(x, read.csv)

for(i in 1:length(file.names)){
  for(j in 2:5){
  df = ls.files[[i]]
  
  df = df[,-c(1:2,9:10)]
  
  
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
    df = df[-c(1:20,nrow(df)),-c(1)]
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    NextOut = NextOut[-c(1:20,length(NextOut))]
    
    df = df[,1:21]
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    # df = round(df, 4)
    
    ###############################
    ############################### BREAK DATA INTO TRAIN AND TEST SETS AND MAKE INTO MATRICES
    set.seed(123)
    sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
    
    train = df[sample.split,]
    test = df[!sample.split,]
    
    train = as.matrix(train)
    test = as.matrix(test)
    
    ###############################
    ############################### SET OUTPUT VALUE
    outcome = NextOut
    
    outcome.train = outcome[sample.split]
    outcome.test = outcome[!sample.split]
    
    ###############################
    ############################### CREATE XG BOOSTED MODLE
    bst = xgboost(data = train,
                  label = outcome.train,
                  objective = "reg:linear",
                  max.depth = 20,
                  nrounds = 200,
                  eta = 0.3,
                  verbose = FALSE)
    pred = predict(bst, test)
    
    compare = data.frame(cbind(outcome.test, pred))
    saveRDS(compare, file = paste0("../bsts-8-3-2023/","compare_",file.names[i],"_",out.names[j-1],".rds"))
    # 
    #   compare$residuals = compare$outcome.test - compare$pred
    #   dmean2 = sum((compare$outcome.test - mean(compare$outcome.test))^2)
    #   dmeanpred2 = sum((compare$pred - mean(compare$outcome.test))^2)
    # 
    #   plot(compare$pred, compare$residuals)
    # 
    #   R2 = dmeanpred2/dmean2
    # 
    #   mse = mean((compare$residuals^2))
    #   rmse = (mean((compare$residuals^2)))^(1/2)
    
    saveRDS(bst, file = paste0("../bsts-8-3-2023/","bst_",file.names[i],"_",out.names[j-1],".rds"))
    print(paste0(file.names[i],"_",out.names[j-1]))
  }
}

toc()
