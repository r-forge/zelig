print.summary.MI <- function(x, subset = NULL, ...){
  m <- length(x$all)
  if (m == 0)
    m <- 1
  if (any(subset > max(m)))
    stop("the subset selected lies outside the range of available \n        observations in the MI regression output.")
  cat("\n  Model:", x$zelig)
  cat("\n  Number of multiply imputed data sets:", m, "\n")
  if (is.null(subset)) {
    cat("\nCombined results:\n\n")
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
    cat("\nFor combined results from datasets i to j, use summary(x, subset = i:j).\nFor separate results, use print(summary(x), subset = i:j).\n\n")
  }
  else {
    if (is.function(subset))
      M <- 1:m
    if (is.numeric(subset))
      M <- subset
    for(i in M){
      cat(paste("\nResult with dataset", i, "\n"))
      print(x$all[[i]], ...)
    }
  }
}

