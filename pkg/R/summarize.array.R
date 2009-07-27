summarize.array <- function(x, rows = NULL, cip, stats, subset = NULL) {
  if (is.function(subset)) { # subset = all; all is class "function"
    res <- apply(x, c(2,3), summarize.default, stats = stats, cip = cip)
    dimnames(res)[[3]] <- rows
  }
  if (is.null(subset)){# subset = NULL; summarizes all obs at once
    tmp <- NULL
    for (j in 1:dim(x)[3])
      tmp <- rbind(tmp, x[,,j])
    res <- apply(tmp, 2, summarize.default,
                 stats = stats, cip = cip)
  }
  if (is.numeric(subset)) { # subset=integer, summarizes identified obs
    if (length(subset) > 1) {
      res <- apply(x[, , subset], c(2,3), summarize.default,
                   stats = stats, cip = cip)
      dimnames(res)[[3]] <- rows
    }
    else 
      res <- apply(x[, , subset], 2, summarize.default,
                   stats = stats, cip = cip)
  }
  res
}
