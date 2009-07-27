param.gee <- function(object, num = NULL, bootstrap = FALSE){
  model <- getzelig(object)
  if (!bootstrap) {
    res <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
  }
  else {
    res <- coef(object)
  }
  res
}


