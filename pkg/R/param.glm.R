param.glm <- function(object, num = NULL, bootstrap = FALSE){
  model <- getzelig(object)
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (model == "normal") {
      df <- object$df.residual
      sig2 <- summary(object)$dispersion
      alpha <- sqrt(df*sig2/rchisq(num, df=df))
      res <- cbind(coef, alpha)
    }
    else if (model == "gamma")  {
      shape <- gamma.shape(object)
      alpha <- rnorm(num, mean = shape$alpha, sd = shape$SE)
      res <- cbind(coef, alpha)
    }
    else if (model == "negbin") {
      alpha <- object$theta
      res <- cbind(coef, c(alpha))
    }
    else
      res <- coef
  }
  else {
    coef <- coef(object)
    if (object$family$family == "gaussian") {
      alpha <- sum(object$residuals^2)/object$df.residual
      res <- c(coef, alpha)
    }
    else if (object$family$family == "Gamma") {
      alpha <- gamma.shape(object)$alpha
      res <- c(coef, alpha)
    }
    else if (object$family$family == "neg.bin") {
      alpha <- object$theta
      res <- c(coef, alpha)
    }
    else
      res <- coef
  }
  res
}





