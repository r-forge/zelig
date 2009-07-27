zelig2relogit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  if (is.null(mf$case.control))
    mf$case.control <- "prior"
  ## transforms y ~ rhs into cbind(y, 1-y) ~ rhs
  mf$formula<- as.formula(call("~", call("cbind",formula[[2]], call("-",1, formula[[2]])), formula[[3]]))
  if (is.null(mf$bias.correct))
    mf$bias.correct <- TRUE
  mf[[1]] <- as.name("relogit")
  as.call(mf)
}
