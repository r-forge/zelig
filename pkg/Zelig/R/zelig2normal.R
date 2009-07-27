zelig2normal <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$M <- mf$robust <- NULL
  mf$model <- FALSE
  mf[[1]] <- glm
  mf$family <- gaussian
  if (is.character(mf$weights))
    mf$weights <- as.name(mf$weights)
  as.call(mf)
}
