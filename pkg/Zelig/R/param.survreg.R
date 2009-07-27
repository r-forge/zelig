param.survreg <- function(object, num, bootstrap = FALSE) {
  cov <- vcov(object)
  coef <- getcoef(object)
  log.scale <- log(object$scale)
  k <- length(coef)
  if(!bootstrap) {
    if(ncol(cov)==k)
      res <- mvrnorm(num, mu=coef, Sigma=cov)
    else 
      res <- mvrnorm(num, mu=c(coef, log.scale), Sigma=cov)
  }
  else {
    if (ncol(cov) == k)
      res <- c(coef)
    else
      res <- c(coef, log.scale)
  }
  res
}
