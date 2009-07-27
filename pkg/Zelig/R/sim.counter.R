sim.counter <- function(object, x, x1=NULL, bootstrap = FALSE, num = c(100, 1000), ...) {
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  else if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  res <- list()
  N <- 0
  for (i in 1:length(x))
    N <- N + nrow(x[[i]])
  for (i in 1:length(x)) {
    numX <- round((nrow(x[[i]]) / N) * num)
    res[[i]] <- sim.default(object, x = x[[i]], x1 = x1[[i]], num =
                            numX, ...)
  }
  res
}

