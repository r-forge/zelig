param.vglm <- function(object, num, bootstrap = FALSE) {
  cov <- vcov(object)
  res <- object@coefficients
  if (!bootstrap) 
    res <- mvrnorm(num, mu=res, Sigma=cov)
  res
}
