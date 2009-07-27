plot.zeliglist <- function(x, xlab = "", user.par = FALSE, ...) {
 
  kk <- length(x)
  j  <- dims(x[[1]]$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(kk, j))
 k <- length(x[[1]]$qi)
  for(i in 1:k){
    for(n in 1:kk){
      xx <- x[[n]]
      class(xx) <- xx$zelig.call$model
      qi <- as.vector(xx$qi[[i]])
     plot(density(qi), main=xx$qi.name[[i]], xlab = xlab, ...)
     
    
    }
  }

}
