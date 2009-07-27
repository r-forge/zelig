zelig2sur <- function(formula, model, data, M,...) {
  check <- library()
  if(any(check$results[,"Package"] == "systemfit")) 
    require(systemfit)
  else
        stop("Please install systemfit using \n	install.packages(\"systemfit\")")
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callsystemfit")
  formula<-parse.formula(formula,model)
  print(formula)
  tt<-terms(formula)
  mf$method<-"SUR"
  mf$model<- mf$M<-NULL
  mf$formula<-formula
  as.call(mf)
}
