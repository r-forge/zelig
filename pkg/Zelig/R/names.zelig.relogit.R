names.zelig.relogit <- function(x) {
   res <- names(unclass(x))[1:5]
   qi.names <- names(x$qi)
   for (i in 1:length(qi.names)) {
     qi.names[i] <- paste("qi$", qi.names[i], sep = "")
  }
  res <- c(res, qi.names)
  res <- list(default = res)
  class(res) <- "names.zelig"
  res
}
