zelig2rq <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots=TRUE)
  tau <- mf$tau
  if(is.null(tau))
    mf$tau<- tau <- 0.5
  
#  if(length(tau) > 1)
#    stop("Zelig does not yet support multiple quantile estimates. Please provide only one value for tau on the interval [0,1].")
#  if(any(tau < 0) || any(tau > 1))
#    stop("Zelig does  not support solutions over the full set of quantiles. Please specify tau on the interval [0,1].")

  mf$model <- FALSE
  mf$se <- NULL #This gets handled by zelig3rq
  mf$M <- NULL
  if (is.character(mf$weights))
    mf$weights <- as.name(mf$weights)

  mf[[1]] <- rq
  as.call(mf)
}
