zelig2w2sls <- function(formula, model, data, M,
                          omit = NULL, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "systemfit")) 
    require(systemfit)
  else
        stop("Please install systemfit using \n	install.packages(\"systemfit\")")
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callsystemfit")
  tmp <- cmsystemfit(formula, omit)
  mf$eqns <- tmp
  mf$method<-"3SLS"
  mf$model<- mf$M<-NULL
  as.call(mf)
}
