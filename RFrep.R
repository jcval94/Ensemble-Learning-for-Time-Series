RFrep<-function(all_data, win1, win2,period,n.pred){
  #Replicamos el modelo
  period2<-period[1:min(4,length(period))]
  period_s2<-period[1]
  period<-period[2]
  
  repr<-floor(n.pred/period)+2
  forec<-c()
  all_data_t<-all_data
  
  #Agrego la predicci?n a la serie y repito el ejercicio de predicci?n
  for(r in 1:repr){
    forec<-c(forec,RFgrid(all_data_t,win1,win2,2,period,period_s2))
    all_data_t<-c(all_data_t,forec)
    #print("inns")
    #Retiramos los primeros periodos, sino se ir? haciendo m?s lenta
    all_data_t<-all_data_t[-(1:period+((r-1)*period))]
  }
  return(forec[1:n.pred])
}
