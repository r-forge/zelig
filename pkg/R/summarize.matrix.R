summarize.matrix <- function(x, rows, cip, stats, subset = NULL) {
  if (is.function(subset)) {
    res <- apply(x, 2, summarize.default, stats = stats, cip = cip)
    colnames(res) <- rows
  }
  if (is.null(subset)) {
    if (length(rows) == 1) {
      res <- apply(x, 2, summarize.default, stats = stats, cip = cip)
      if (length(res) == 1)
        names(res) <- as.character(unique(x))
    }
    else {
      tmp <- NULL
      for (i in 1:dim(x)[2])
        tmp <- c(tmp, x[,i])
      res <- summarize.default(tmp, stats = stats, cip = cip)
    }
  }
  if (is.numeric(subset)) {
    res <- apply(as.matrix(x[,subset]), 2, summarize.default,
                 cip = cip, stats = stats)
    colnames(res) <- rows
  }
  res
}
