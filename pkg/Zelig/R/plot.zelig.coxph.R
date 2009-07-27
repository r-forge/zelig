plot.zelig.coxph <- function(x, xlab = "", user.par = FALSE, alt.col = "red", alt.lty = "dashed", CI = 95, ...) {
  s <- summary(x, CI=CI)
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  if(!is.null(x$qi$hr)){
  	hr <- x$qi$hr
  	plot(density(hr), main = x$qi.name$hr, xlab = xlab, ...)
  	s$qi.stats$hr <- x$qi.name$hr <- NULL
	j <- k-1
  }
  else{
	j <- k
  }
  for (i in 1:(j-1)) {
    qi <- as.vector(s$qi.stats[[i]][,1])
    time <- as.numeric(rownames(s$qi.stats[[i]]))
    ci.lower <- as.vector(s$qi.stats[[i]][,3])
    ci.upper <- as.vector(s$qi.stats[[i]][,4])
    plot(y = qi, x = time, main = x$qi.name[[i]], xlab = "time", ylab = names(x$qi.name)[i], type = "s", ...)
    lines(y = ci.lower, x = time, type = "s", lty = alt.lty, col = alt.col)
    lines(y = ci.upper, x = time, type = "s", lty = alt.lty, col = alt.col)
  }
  haz <- as.vector(s$qi.stats[[j]])
  time <- as.numeric(rownames(s$qi.stats[[j-1]]))
  plot(y = haz, x = time, main = x$qi.name[[j]], xlab = "time", ylab = names(x$qi.name)[j], type = "s")
  par(op)
}
