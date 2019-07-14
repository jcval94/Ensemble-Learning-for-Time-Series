ARIMA_M<-function(Vec,n=9,Tend=F){
  #decomponer el vector en trimestres
  freq = Period(Vec)[[1]]
  freq<-max(freq[freq<length(Vec)/2])
  
  if(length(freq)==0){
    freq<-12
    # return(NULL)
    # break("")
  }
  
  dec<-stl(ts(Vec,freq=freq),s.window = "per")$time.series
  #extraemos la parte de tendencia
  dec_t <- dec[,2]
  #Ajusta un arima a la tendencia
  if(Tend){
    AA<-forecast::auto.arima(dec_t)
  }
  else{
    AA<-forecast::auto.arima(rowSums(dec[,1:3]))
  }
  ARIMA<-AA
  
  #Si el modelo es (0,0,0), se puede elegir que tenga al menos un PAR?Metro.
  if(sum(purrr::map_int(AA$model[1:3],length))==0){
    MM<-data.frame()
    nuu<-0
    errores<-c()
    for (p in 0:4) {
      for (i in 0:1) {
        for (q in 0:4) {
          if(sum(p,i,q)>=sumaparm){
            suppressWarnings(Err<-try(MM<-rbind(MM,data.frame(p=p,i=i,q=q,
                                                              AIC=arima(dec_t,c(p,i,q))$aic,
                                                              Likehood=arima(dec_t,c(p,i,q))$loglik)),
                                      silent = T))
            nuu<-nuu+1
            if(is.error(Err)){
              errores<-c(errores,num)
              errores<-unique(errores)
              next()}
          }
        }
      }
    }
    #Retiramos los que arrojaron error
    if(length(errores)>0 & sum(errores)!=0){MM<-MM[-unique(errores),]}
    
    #Guarda aquel con menor AIC
    MM[MM$AIC==(min(MM$AIC)),]->BST
    BST<-BST[1,]
    #AIC es nuestro criterio
    ARIMA<-arima(dec_t,c(BST$p,BST$i,BST$q))
  }
  qt<--(1:(length(Vec)%%freq))
  estac<-(rep(dec[,1][1:freq],floor(n/freq)+2)[qt])[1:n]
  
  #Modelo ARIMA + componente estacional+media del reminder
  Err_ARIMA<-try(pobla<-(as.numeric(predict(ARIMA,n.ahead = n)$`pred`))+estac+mean(dec[,3]),silent = T)
  
  if(assertthat::is.error(Err_ARIMA)){
    pobla<-(as.numeric(forecast::forecast(ARIMA,n)$mean))+estac+mean(dec[,3])
  }
  
  return(list(pobla,ARIMA))
}
