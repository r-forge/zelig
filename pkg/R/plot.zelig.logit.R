plot.zelig.logit <- function(x, xlab = "", user.par = FALSE, alt.col = "red", ...){
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  pr <- x$qi$pr
  y0 <- 100 * sum(pr == 0)/length(pr)
  y1 <- 100 * sum(pr == 1)/length(pr)
  barplot(c(y0, y1), horiz = TRUE, col = alt.col, las = 1,
          names.arg = c("Y = 0", "Y = 1"),
          xlab = "Percentage of Simulations",
          main = x$qi.name$pr, xlim = c(0, 100))
  x$qi$pr <- x$qi.name$pr <- NULL
  for (i in 1:(k-1)) {
    qi <- as.vector(x$qi[[i]])
    plot(density(qi), main = x$qi.name[[i]], xlab = xlab, ...)
  }
  par(op)
}
