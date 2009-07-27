names.summary.zelig <- function(x) {
  res <- names(unclass(x))[1:4]
  qi.names <- names(x$qi.stats)
  for (i in 1:length(qi.names)) {
    qi.names[i] <- paste("qi.stats$", qi.names[i], sep = "")
  }
  res <- c(res, qi.names)
  res <- list(default = res)
  class(res)<-"names.zelig"
  res
}
