setx.rq <- function(object, fn=list(numeric=mean, ordered=median, other=mode), data=NULL, cond=FALSE, counter=NULL, ...) {
  mc <- match.call(expand.dots=T)
  env <- parent.frame()
  if(cond==TRUE)
    stop("Conditional prediction is not supported for quantile regression. Please set cond=FALSE.")
  mc[[1]] <- setx.default
  return(eval(mc, env=env))
}
