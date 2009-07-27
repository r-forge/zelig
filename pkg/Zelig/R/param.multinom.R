param.multinom <- function(object, num, bootstrap = FALSE) {
  k <- length(object$lev)
  coef <- NULL
  tmp <- coef(object)
  for (i in 1:(k-1))
    coef <- c(coef, tmp[i,])
  if (!bootstrap) 
    sim.coef <- mvrnorm(num, mu=coef, Sigma=vcov(object))
  else
    sim.coef <- coef
  sim.coef
}
