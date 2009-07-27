param.multiple <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (object$zelig %in% c("sur","2sls","w2sls","3sls")) {
      res <- coef
    }
    else
      res <- coef
  }
  else {
    coef <- coef(object)
      res <- coef
  }
  res
}




