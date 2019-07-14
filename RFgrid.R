RFgrid <- function(data_train, param1, param2, K=2, period, period_s2){
  
  if(missing(period)){
    period = Period(data_train)[[1]]
  }
  if(length(period)>1){period<-period[1]}
  
  N <- length(data_train)
  
  perds<-unique(c(period,period_s2))
  data_ts <- msts(data_train, seasonal.periods = perds)
  
  #Se crean variables senoidales y cosenoidales de las estacionalidades
  #de la serie y se generan h periodos a futuro para hacer el test
  #Hago que K est? bien definida
  K<-ifelse(sum(perds/K<2)>0,min(perds[perds/K<2])/K-.01,K)
  
  fuur <- fourier(data_ts, K = rep(K,length(perds)))
  fuur_test <- as.data.frame(fourier(data_ts, K = rep(K,length(perds)), h = max(perds)))
  #El periodo ser? el m?ximo de los periodos tal que divida al train en 2 o m?s segmentos
  
  #period<-min(c(period,period_s2))
  window <- (N / period) - 1
  data_ts <- ts(data_train, freq = period)
  #print("AA")
  decomp_ts <- stl(data_ts, s.window = "per", robust = TRUE)
  #Serie sin tendencia
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  
  #La tendencia se ajusta a un arima, se proyecta n periodos a futuro
  #Y se obtiene la media de dichos periodos
  trend_part <- ts(decomp_ts$time.series[,2])
  trend_fit <- forecast::auto.arima	(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  per.win<-round(period*window,0)
  #Se obtienen los n-1 periodos del componente estacional
  lag_seas <- decomp_ts$time.series[1:(per.win), 1]
  
  #Se genera el df del train y se crea un RF con este
  matrix_train <- data.frame(Load = tail(new_load, (per.win)),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  #Se explicar? la serie sin tendencia V?a un RF
  tree_2 <- randomForest(Load ~ ., data = matrix_train,
                         ntree = 80, mtry = param1, nodesize = param2, importance = TRUE)
  #ntree se queda en 730, pues es un n?mero suf. gde para que el modelo haya alcanzado su ntree ?ptimo
  #Trae el complemento de lag_seas
  test_lag <- decomp_ts$time.series[((per.win)+1):N, 1]
  
  #Se genera el df para proyectar
  matrix_test <- data.frame(fuur_test[1:period,],
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}
