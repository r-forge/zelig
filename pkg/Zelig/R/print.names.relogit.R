print.names.relogit<-function(x, ...){
  if (length(x$tau) == 2) {
    print(x$default, ...)
    cat(paste("Additional objects available in lower.estimate and upper.estimate: \n"))
    print(x$estimate)
  }
  else
    print(x$default, ...)
}

