zelig2coxph <- function(formula, model, data, M, ...) {
  require(survival)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- survival::coxph
  if (is.null(mf$robust))
    mf$robust <- FALSE
  if (!is.null(mf$cluster) & !mf$robust)
    stop("\nIf cluster is specified, robust must be TRUE.")
  if (!is.null(mf$cluster)) {
    mf$formula <- update(mf$formula, paste(". ~ . + ",
                                           paste("cluster(",mf$cluster,")")))
    mf$cluster <- NULL
  }
    #mf$formula <- as.formula(paste(paste(deparse(formula[[2]])),
     #                              paste(deparse(formula[[1]])),
      #                             paste(deparse(formula[[3]], width.cutoff=500)),
       #                            paste("+", " cluster(",
        #                                 mf$cluster, ")")))
    #mf$cluster <- NULL
  #}
  as.call(mf)
}

