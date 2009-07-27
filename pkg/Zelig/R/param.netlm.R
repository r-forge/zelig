param.netlm <- function(object, num, bootstrap = FALSE) {
#  if (num < 1) num <- 1
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov.netlm(object))
    df <- object$df.residual
    sig2 <- summary(object)$sigma^2
    alpha <- sqrt(df*sig2/rchisq(num, df=df))
    res <- cbind(coef, alpha)
  }
  else {
    coef <- coef(object)
    alpha <- summary(object)$sigma
    res <- c(coef, alpha)
  }
  res
}