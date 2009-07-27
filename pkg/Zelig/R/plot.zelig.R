plot.zelig <- function(x, xlab = "", user.par = FALSE, ...) {
  if (dim(x$x)[1] > 1) 
      plot.ci(x, xlab = "", ...)
  else {
    class(x) <- x$zelig.call$model
    if (exists(paste("plot.zelig", x$zelig.call$model, sep = ".")))
      UseMethod("plot.zelig", x)
    else{
      res <- try(plot.zelig.default(x, xlab = xlab, user.par = user.par, ...), silent=F)
      if(class(res) =="try-error")
        message("No plot generated for model ", class(x)[1]) 
    }
  
  }
}
