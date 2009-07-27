summary.coxph.naive <- function(object, ...){
  class(object) <- c("coxph")
  res <- summary(object,...)
  naivese <- res$coef[,3]
  expcoef <- res$coef[,2]
  res$coef[,2] <- naivese
  res$coef[,3] <- expcoef
  colnames(res$coef)[2] <- "se(coef)"
  colnames(res$coef)[3] <- "exp(coef)"
  class(res) <- c("summary.coxph.naive", "summary.coxph")
  return(res)
}
