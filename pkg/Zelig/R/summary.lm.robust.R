summary.lm.robust <- function(object, ...) {
  class(object) <- "lm"
  res <- summary.lm(object, ...)
  if (is.null(object$robust)) {
    res$cov.unscaled <- R <- vcovHC(object)/(res$sigma^2)
    res$robust <- "vcovHC"
  }
  else {
    fn <- object$robust$method
    res$robust <- object$robust$method
    object$robust$method <- NULL
    arg <- object$robust
    arg$x <- object
    res$cov.unscaled <- R <- eval(do.call(fn, arg))/(res$sigma^2)
  }
  res$coefficients[,2] <- se <- sqrt(diag(R))*res$sigma
  if (!is.null(res$correlation)) {
    res$correlation <- (R * res$sigma^2)/outer(se, se)
    dimnames(res$correlation) <- dimnames(res$cov.unscaled)
  }
  res$coefficients[,3] <- tval <- coefficients(object)/se
  res$coefficients[,4] <- 2*pt(abs(tval), res$df[2], lower.tail =
                               FALSE)
  class(res) <- c("summary.lm.robust", "summary.lm")
  return(res)
}
