gridSearch <- function(Y,  FUN, param1, param2, period = Period(Y)[[1]]) {
  param3<-period
  days <- length(Y)#"d?as" u obs. totales ingresadas
  period_s2<-period[2]
  period<-period[1]
  #Usaremos el n?mero de periodos equivalente al .75 de las obs
  #S no alcanza, usaremos lo que resta del periodo
  periodos_a_usar<-max(sum(seq(period,days,by=period)/days<.75),3)
  
  test.days <- days - floor(period)*periodos_a_usar #longitud de observaciones con las que entrenar?
  if(test.days==0){
    param3<-period<-floor(period/2)
    test.days <- days - floor(period)*periodos_a_usar
  }
  #matriz de PAR?Metros 
  mape.matrix <- matrix(0, nrow = length(param1), ncol = length(param2))
  row.names(mape.matrix) <- param1
  colnames(mape.matrix) <- param2
  #Vector de predicciones que ir? llenandose
  #vector(length = test.days*period)
  
  #Hiperparametrizaci?n del bosque
  for(i in seq_along(param1)){
    for(j in seq_along(param2)){
      forecast.rf <- c()
      for(k in 0:(test.days/period)){
        train.set <- Y[((period*k)+1):((period*k)+(periodos_a_usar*floor(period)))]
        if(period/length(train.set)>.5){
          break()
        }
        forecast.rf <- c(forecast.rf,FUN(data_train = train.set, param1 = param1[i], param2 = param2[j],
                                         period=period,period_s2=period_s2))
        
      }
      #Real vs predicci?n
      #print(length(forecast.rf))
      real<-Y[-(1:(periodos_a_usar*floor(period)))]
      #print(len(real))
      mape.matrix[i,j] <- mape(real, forecast.rf[1:length(real)])
    }
  }
  return(list(mape.matrix,param3))
}
