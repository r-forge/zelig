plot.zelig.3sls <- function(x, xlab = "", user.par = FALSE, ...) {
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, dims(x$qi[[1]])[2]))
  for (i in 1:k) {
    for (j in 1:dims(x$qi[[i]])[2]){
      qi <- as.vector((x$qi[[i]])[,j])
      plot(density(qi), main = x$qi.name[[i]], xlab = xlab, ...)
    }
  }
  par(op)
}
