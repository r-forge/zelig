summarize.ei <- function(x, rows = NULL, cip, stats, subset = NULL) {
  if (is.function(subset)) { # subset = all; all is class "function"
    res <- apply(x, c(2,3,4), summarize.default, stats = stats, cip = cip)
    dimnames(res)[[4]] <- rows
  }
  if (is.null(subset)){# subset = NULL; summarizes all obs at once
    tmp <- NULL
    tmp <- apply(x, c(2,3), rbind, tmp)
    res <- apply(tmp, c(2,3), summarize.default,
                 stats = stats, cip = cip)
  }
  if (is.numeric(subset)) { # subset=integer, summarizes identified obs
    if (length(subset) > 1) {
      res <- apply(x[, , , subset], c(2,3,4), summarize.default,
                   stats = stats, cip = cip)
      dimnames(res)[[4]] <- rows
    }
    else 
      res <- apply(x[, , subset], 2, summarize.default,
                   stats = stats, cip = cip)
  }
  dimnames(res)[2:3] <- dimnames(x)[2:3] 
  res
}
