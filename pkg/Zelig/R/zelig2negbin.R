zelig2negbin <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  mf[[1]] <- MASS::glm.nb
  if (is.character(mf$weights))
    mf$weights <- as.name(mf$weights)
  as.call(mf)
}
