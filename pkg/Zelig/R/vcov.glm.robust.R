vcov.glm.robust <- function(object, ...) {
  so <- summary.glm.robust(object, corr=FALSE, ...)
  so$dispersion * so$cov.unscaled
}
