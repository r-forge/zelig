bootfn.default <- function(data, i, object) {
  d <- data[i,]
  object$call$data <- d
  l <- length(param(object, bootstrap = TRUE))
  fit <- eval(object$call, sys.parent())
  l1 <- length(param(fit, bootstrap = TRUE))
  while (l > l1) {
    object$call$data <- data[sample(nrow(data), replace=TRUE),]
    fit <- eval(object$call, sys.parent())
    l1 <- length(param(fit, bootstrap = TRUE))
  }
  param(fit, bootstrap = TRUE)
}
