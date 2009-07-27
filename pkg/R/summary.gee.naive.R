summary.gee.naive <- function(object, ...){
  class(object) <- c("gee", "glm")	
  ##res <- summary.gee(object,...)
  res <- summary(object,...)
  res$coefficients <- res$coefficients[,-5]	
  res$coefficients[,2] <- s.err <- sqrt(diag(object$naive.variance))	
  res$coefficients[,3] <- zvalue <- coef(object)/s.err	
  res$coefficients[,4] <- 2 * pnorm(-abs(zvalue))	
  colnames(res$coefficients) <- c("Estimate", "Naive SE", "z value", "Pr(>|z|)")
  class(res) <- c("summary.gee.naive", "summary.gee")
  return(res)
}
