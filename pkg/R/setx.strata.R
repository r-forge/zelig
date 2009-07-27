setx.strata <- function(object, fn = list(numeric = mean, ordered =
                                    median, other = mode), data =
                          NULL, cond = FALSE, counter = NULL, ...) {
  obj <- object[[1]]
  x <- list()
  if (any(class(obj)=="MI")) { # with multiple imputation
    if (is.null(data))
      dta <- eval(getcall(obj[[1]])$data, sys.parent())
    else
      dta <- data
    by <- getcall(obj[[1]])$by
    M <- length(dta)
    d <- dta[[1]]
    idx <- pmatch(by, names(d))[1]
    lev <- sort(unique(eval(d[[idx]], sys.parent())))
    for (i in 1:length(lev)) {
      d <- list()
      for (j in 1:M) {
        dM <- dta[[j]]
        d[[j]] <- as.data.frame(dM[dM[[idx]] == lev[i],])
      }
      x[[i]] <- setx(object[[i]], fn = fn, data = d, cond = cond,
                     counter = counter, ...)
    }
  }
  else { # without multiple imputation
    if (is.null(data))
      dta <- eval(getcall(obj)$data, sys.parent())
    else
      dta <- data
    by <- getcall(obj)$by
    idx <- pmatch(by, names(dta))[1]
    lev <- sort(unique(eval(dta[[idx]], sys.parent())))
    for (i in 1:length(lev)) {
      d <- as.data.frame(dta[dta[[idx]] == lev[i],])
      x[[i]] <- setx(object[[i]], fn = fn, data = d, cond = cond,
                   counter = counter, ...)
    }
  }
  names(x) <- names(object)
  class(x) <- c("setx.strata", "data.frame")
  return(x) 
}




