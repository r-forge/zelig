zelig2ei.RxC <- function(formula, model, data, M, covar= NULL, ...) {

  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callparamseiestim")
  mf$model<-mf$M<-NULL
  as.call(mf)
}
