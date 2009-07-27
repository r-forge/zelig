summary.glm.robust <- function(object, ...) {
  class(object) <- c("glm", "lm")
  res <- summary.glm(object, ...)
  if (is.null(object$robust)) {
    res$cov.unscaled <- covmat.unscaled <- vcovHAC(object)
    res$robust <- "vcovHAC"
  } else {
    fn <- object$robust$method
    res$robust <- object$robust$method
    object$robust$method <- NULL
    arg <- object$robust
    arg$x <- object
    res$cov.unscaled <- covmat.unscaled <- eval(do.call(fn, args=arg))
  }
  res$cov.scaled <- covmat <- covmat.unscaled*res$dispersion
  if (!is.null(res$correlation)) {
    dd <- sqrt(diag(res$cov.unscaled))
    res$correlation <- res$cov.unscaled/outer(dd, dd)
    dimnames(res$correlation) <- dimnames(res$cov.unscaled)
  }

  res$coefficients[,2] <- s.err <- sqrt(diag(covmat))
  res$coefficients[,3] <- tvalue <- coefficients(object)/s.err
  if (length(dimnames(res$coefficients)[[2]])>3) {
    if (dimnames(res$coefficients)[[2]][3]=="z value")
      res$coefficients[,4] <- 2 * pnorm(-abs(tvalue))
    else
      res$coefficients[,4] <- 2 * pt(-abs(tvalue), object$df.residual)
  }
  class(res) <- c("summary.glm.robust","summary.glm")
  return(res)
}
