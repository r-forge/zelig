param.netglm <- function(object, num = NULL, bootstrap = FALSE, x = NULL) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (getzelig(object) == "normal.net") {
      df <- object$df.residual
      sig2 <- summary(object)$dispersion
      alpha <- sqrt(df*sig2/rchisq(num, df=df))
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "gamma.net")  {
      class(object) <- c("glm","lm")
	  rate <- gamma.shape(object) 
	  class(object) <- "netglm"
      alpha <- rnorm(num, mean = rate$alpha, sd = rate$SE)
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "netnegbin") {
      alpha <- object$theta
      res <- cbind(coef, c(alpha))
    }
    else
      res <- coef
  }
  else {
    coef <- coef(object)
    if (object$family$family == "normal.net") {
      alpha <- sum(object$residuals^2)/length(object$residuals)
      res <- c(coef, alpha)
    }
    else if (object$family$family == "gamma.net") {
      alpha <- gamma.dispersion(object)
      res <- c(coef, alpha)
    }
    else if (object$family$family == "netnegbin") {
      alpha <- object$theta
      res <- c(coef, alpha)
    }
    else
      res <- coef
  }
  res
}
