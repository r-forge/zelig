param.gam <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (getzelig(object) == "normal.gam") {
      df <- object$df.residual
      sig2 <- summary(object)$dispersion
      alpha <- sqrt(df*sig2/rchisq(num, df=df))
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "gamma.gam")  {
      rate <- gamma.shape(object) 
      alpha <- rnorm(num, mean = rate$alpha, sd = rate$SE)
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "negbin.gam") {
      alpha <- object$theta
      res <- cbind(coef, c(alpha))
    }
    else
      res <- coef
  }
  res
}




