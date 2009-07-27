summary.gee.robust <- function(object, ...){
  class(object) <- c("gee", "glm")
  ##res <- summary.gee(object,...)
  res <- summary(object,...)	
  res$coefficients <- res$coefficients[,-5]
  res$coefficients[,2] <- s.err <- sqrt(diag(object$robust.variance))
  res$coefficients[,3] <- zvalue <- coef(object)/s.err
  res$coefficients[,4] <- 2 * pnorm(-abs(zvalue))
  colnames(res$coefficients) <- c("Estimate", "Robust SE", "z value", "Pr(>|z|)")
  class(res) <- c("summary.gee.robust", "summary.gee")
  return(res)
}
