vcov.netlm <- function(object, ...) {
  so <- summary.lm(object, corr = FALSE, ...)
  so$sigma^2 * so$cov.unscaled
}
