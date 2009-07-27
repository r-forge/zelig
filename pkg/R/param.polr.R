param.polr <- function(object, num, bootstrap=FALSE) {
  coef <- object$coefficients
  zeta <- object$zeta
  k <- length(coef)
  if (!bootstrap) {
    theta <- NULL
    theta[1] <- zeta[1]
    for (i in 2:length(zeta)) 
      theta[i] <- log(zeta[i] - zeta[i-1])
    res <- mvrnorm(num, mu=c(coef,theta), Sigma=vcov(object))
  }
  else
    res <- c(coef, zeta)
  res
}
