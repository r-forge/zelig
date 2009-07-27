plot.zelig.poisson.gee <- function(x, xlab = "", user.par = FALSE, ...) {
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  plot(density(x$qi$ev), main = x$qi.name$ev, xlab = xlab, ...) 
  if (k > 2) {
    for (i in 3:k)
      plot(density(x$qi[[i]]), main = x$qi.name[[i]],
           xlab = xlab, ...)
  }
  par(op)
}

