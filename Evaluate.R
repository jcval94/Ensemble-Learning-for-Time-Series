Evaluate<-function(X,npred=floor(length(X)/4)){
  X<-ifelse(is.nan(X),1,X)
  if(length(X)<15){
    return("Length data must be > 15")
  }
  p<-Period(X)
  
  #Descartamos las Periodes tal que 
  desc<-length(X)/p[[1]]<3
  #si no, no se podr? hacer la descomposici?n
  p[[1]]<-unique(round(p[[1]][!desc],0))
  p[[1]]<-p[[1]][p[[1]]!=2]
  
  if(length(p[[1]])==1 && p[[1]]!=12){p[[1]]<-c(p[[1]],12)}else{p[[1]]<-c(p[[1]],6)}

  mp_2<-c()
  res_T<-list()
  all_data <- X
  for (i in 1:min(4,max(length(p[[1]])-1,1))){
    #Buscaremos aquel modelo que arroje el menor de los errores
    #En caso de que la Period abarque 2 o menos 
    per<-p[[1]][1:2+(i-1)]
    E1<-try(res_1 <- gridSearch(Y = all_data, FUN = RFgrid, param1 = c(2,3,4,5), param2 = c(2,3,4,5),period = per),silent=T)
    #De los periodos obtenidos elegir el de menor error
    if(is.error(E1) | any(is.na(E1[[1]]))){next()}
    res_T[[i]]<-res_1
    data_grid <- (reshape2::melt(res_1[[1]]))
    #Dado que esta fue la mejor parametrizaci?n, ser? la que usaremos
    winners<-as.numeric(data_grid[data_grid$value==min(data_grid$value),1:2])
    mp_2[i]<-(min(data_grid$value))
    print(i)
  }
  print("Fin Fases")
  res_Win<-res_T[ifelse(is.na(mp_2==min(mp_2,na.rm = T)),F,mp_2==min(mp_2,na.rm = T))][[1]]
  
  period<-res_Win[[2]]
  data_grid <- (reshape2::melt(res_Win[[1]]))
  winners<-as.numeric(data_grid[data_grid$value==min(data_grid$value),1:2])
  
  pr_p<-RFrep(all_data,winners[1],winners[2],period = period,n.pred=npred)
  
  return(list(pr_p,mp_2))
}
