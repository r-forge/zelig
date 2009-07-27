zelig2oprobit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- MASS::polr
  mf$Hess <- TRUE
  mf$method <- "probit"
  if (is.character(mf$weights))
    mf$weights <- as.name(mf$weights)
  as.call(mf)
}
