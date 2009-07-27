model.end <- function(res, mf) {

  res$variance <- -solve(res$hessian)
  res$hessian <- NULL

  colnames(res$variance) <- rownames(res$variance) <- names(res$par)
  res$coefficients <- res$par
  res$par <- NULL

  res$terms <- attr(mf, "terms")

  attr(res, "na.message") <- attr(mf, "na.message") 
  if (!is.null(attr(mf, "na.action"))) 
    res$na.action <- attr(mf, "na.action") 

  res
}
