zelig2weibull <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL

  mf[[1]] <- survival::survreg
  mf$dist <- "weibull"
  if (is.null(mf$robust))
    mf$robust <- FALSE
  if (!is.null(mf$cluster) & !mf$robust)
    stop("\nIf cluster is specified, robust must be TRUE.")
  
  if (!is.null(mf$cluster)) {
          mf$formula <- update(mf$formula, paste(". ~ . + ", paste("cluster(",mf$cluster,")")))
          mf$cluster <- NULL
  } else if (mf$robust)
    mf$formula <- update(formula, paste(". ~ . + ", paste("cluster(1:nrow(",deparse(formula[[2]]),"))")))
  as.call(mf)
}
