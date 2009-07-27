callsystemfit<-function(formula,data,method,inst=NULL,...){
  t<-terms.multiple(formula)
  out<-systemfit(data=data,eqns=formula,method=method,inst=inst,...)
  attr(out,"terms")<-t
  class(out)<-c(class(out),"multiple")
  return (out)
}
