arima.wrap <- function(formula, order, x, xreg=NULL, data, M, ...){
  t<- terms(formula)
	out<-arima(x=x, xreg=xreg, order=order, ...)
  out$terms<- t
  class(out)<- c(class(out), "Arima")
  return(out)
} 
