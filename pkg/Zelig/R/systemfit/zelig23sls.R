zelig23sls <- function(formula, model, data, M,...) {
  check <- library()
  if(any(check$results[,"Package"] == "systemfit")) 
    require(systemfit)
  else
        stop("Please install systemfit using \n	install.packages(\"systemfit\")")
  "%w/o%" <- function(x,y) x[!x %in% y]
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callsystemfit")
  formula<-parse.formula(formula,model)
  tt<-terms(formula)
  ins<-names(tt) %w/o% names(attr(tt,"depVars"))
  if(length(ins)!=0)
    if(length(ins)==1)
      inst<-formula[[ins]]
    else inst<-formula[ins]
  else
    stop("2sls model requires instrument!!\n")
  mf$method<-"3SLS"
  mf$inst<-inst
  mf$model<- mf$M<-NULL
  mf$formula<-formula[names(attr(tt,"depVars"))]
  class(mf$formula)<-c("multiple","list")
  as.call(mf)
}
