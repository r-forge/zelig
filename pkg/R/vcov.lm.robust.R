vcov.lm.robust <- function(object, ...) {
  so <- summary.lm.robust(object, corr=FALSE, ...)
  so$cov.unscaled * so$sigma^2
}
