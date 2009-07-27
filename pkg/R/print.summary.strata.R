print.summary.strata <- function(x, subset = NULL, ...){
  if (is.null(subset))
    m <- length(x$M)
  else if (any(subset > max(m)))
    stop("the subset selected lies outside the range of available \n        sets of regression output.")
  else
    m <- subset
  cat("\n  Model:", x$call$model)
  cat("\n  Number of subsets evaluated:", m, "\n")
  for (i in 1:m) {
    cat(paste("\nResults for", x$by[1], "=", x$lev[i], "\n"))
    print(x[[i]])
    cat("\n")
  }
}

