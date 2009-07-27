summarize.default <- function(x, rows = NULL, cip, stats, subset = NULL) {
  res <- NULL
  if (is.numeric(x)) {
    for (i in 1:length(stats))
      res <- c(res, do.call(stats[i], list(x)))
    res <- c(res, quantile(x, cip))
    names(res) <- c(stats, paste(cip*100, "%", sep = ""))
  }
  else if (is.character(x)) {
    res <- c(table(x) / length(x))
  }
  res
}
