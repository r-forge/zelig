zelig2ls <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- FALSE
  mf$M <- mf$robust <- NULL
  if (is.character(mf$weights))
    mf$weights <- as.name(mf$weights)
  
  mf[[1]] <- lm
  as.call(mf)
}
