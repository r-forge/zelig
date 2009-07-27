print.summary.zelig.strata <- function(x, subset = NULL, ...){
  if (is.null(subset))
    m <- length(x)
  else if (any(subset > max(m)))
    stop("the subset selected lies outside the range of available \n        sets of regression output.")
  else
    m <- subset
  for (i in 1:m) {
    cat(paste("\nResults for", names(x)[i], "\n"))
    print(x[[i]])
    cat("\n")
  }
}

