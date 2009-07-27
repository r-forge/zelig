param.default <- function(object, num, bootstrap = FALSE) {
  if (!bootstrap)
    res <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
  else
    res <- coef(object)
  res
}
