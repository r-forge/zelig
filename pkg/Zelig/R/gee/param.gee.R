param.gee <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (object$zelig == "normal.gee") {
      sig2 <- object$scale
      alpha <- sqrt(sig2)
      res <- cbind(coef, alpha)
    }
    else if (object$zelig == "gamma.gee")  {
      alpha<-1/object$scale
      res <- cbind(coef, alpha)
    }
    else
      res <- coef
  }
  else {
    coef <- coef(object)
    if (object$family$family == "gaussian") {
      alpha <- sum(object$residuals^2)/length(object$residuals)
      res <- c(coef, alpha)
    }
    else if (object$family$family == "Gamma") {
      alpha <- object$scale
      res <- c(coef, alpha)
    }
    else
      res <- coef
  }
  res
}




