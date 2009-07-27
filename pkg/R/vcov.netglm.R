vcov.netglm <- function(object, ...)
{
    so <- summary.glm(object, corr=FALSE, ...)
    so$dispersion * so$cov.unscaled
}