summary.relogit <- function(object, ...) {

  dta <- model.matrix(terms(object), data=model.frame(object))
  class(object) <- class(object)[2]
  res <- summary(object, ...)
  if (object$bias.correct) {
    n <- nrow(dta)
    k <- ncol(dta)
    res$cov.unscaled <- res$cov.unscaled * (n/(n+k))^2
    res$cov.scaled <- res$cov.unscaled * res$dispersion
    res$coef[,2] <- sqrt(diag(res$cov.scaled))
    res$coef[,3] <- res$coef[,1] / res$coef[,2]
    res$coef[,4 ] <- 2*pt(-abs(res$coef[,3]), res$df.residual)
  }
  res$call <- object$call
  res$tau <- object$tau
  res$bias.correct <- object$bias.correct
  res$prior.correct <- object$prior.correct
  res$weighting <- object$weighting
  class(res) <- "summary.relogit"
  return(res)
}












