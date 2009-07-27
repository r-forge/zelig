param.lm <-function(object, num, bootstrap = FALSE) {
#  if (num < 1) num <- 1
 
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    df <- object$df.residual
    sig2 <- summary.lm(object)$sigma^2
    alpha <- sqrt(df*sig2/rchisq(num, df=df))
    res <- cbind(coef, alpha)
  }
  else {
    coef <- coef(object)
    alpha <- summary.lm(object)$sigma
    res <- c(coef, alpha)
  }
  res
}
