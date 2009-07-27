setx.eiRxC <- function(object, fn = list(numeric = mean, ordered =
                                   median, other = mode), data = NULL,
                         cond = FALSE, counter = NULL, ...)
{
  if (!is.null(object$covar)){
    object1 <- object
    #object1$call$data<-as.data.frame(object$covar);
    origCall <- getcall(object)
    covFormula<-eval(origCall[["covar"]])
    object1$terms <- terms.formula(covFormula)
    res<- setx.default(object=object1, fn=fn, data=data, cond=cond, counter=counter,...)
    return (res[,2:ncol(res)])
  } else
  {
 return(setx.default(object,fn=NULL))
 }
}
