plot.zelig.logit.gee <- function(x, xlab = "", user.par = FALSE, alt.col = "red", ...){
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
    par(mfrow = c(k, 1))
  if (k > 1){
    for (i in 1:k) {
      qi <- as.vector(x$qi[[i]])
      plot(density(qi), main = x$qi.name[[i]], xlab = xlab, ...)
    }
  }
  else{
    plot(density(x$qi$ev), main = x$qi.name$ev, xlab = xlab, ...) 
  }
  par(op)
}
