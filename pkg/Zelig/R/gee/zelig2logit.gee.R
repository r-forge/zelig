zelig2logit.gee <- function(formula, model, data, M, ...) {
  require(gee) || stop("install gee using...")
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("gee")
  mf$family <- as.name("binomial")
  as.call(mf)
}
