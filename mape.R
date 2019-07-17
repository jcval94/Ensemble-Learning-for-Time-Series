mape <- function(real, pred){
  return(mean(abs(1-pred/real)[real!=0]))
}

##Random variables example

A<-rnorm(1000,1,0)
B<-rnorm(1000,1,1)

mape(A,B)

##RModel example...
