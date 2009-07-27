summary.coxph.robust <- function(object, ...){
  class(object) <- c("coxph")
  res <- summary(object,...)
  robustse <- res$coef[,4]
  expcoef <- res$coef[,2]
  res$coef[,2] <- robustse
  res$coef[,4] <- expcoef
  colnames(res$coef)[2] <- "robust se"
  colnames(res$coef)[4] <- "exp(coef)"
  class(res) <- c("summary.coxph.robust", "summary.coxph")
  return(res)
}



