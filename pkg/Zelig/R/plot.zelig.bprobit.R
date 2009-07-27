plot.zelig.bprobit <- function(x, xlab = "", user.par = FALSE, alt.col = "red", ...){
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par)
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  if ("rr" %in% names(x$qi)) {
    x$qi$rr <- x$qi.name$rr <- NULL
    k <- k - 1
  }
  main <- x$qi.name
  par(mfrow = c(k, 1))
  for (i in 1:k) {
    qi <- as.matrix(x$qi[[i]])
    if (names(x$qi)[i] == "pr") { 
      total <- sum(as.integer(qi))
      y00 <- 100 * sum(as.integer(qi[,1]))/total
      y01 <- 100 * sum(as.integer(qi[,2]))/total
      y10 <- 100 * sum(as.integer(qi[,3]))/total
      y11 <- 100 * sum(as.integer(qi[,4]))/total
      xmax <- max(y00, y01, y10, y11)
      labels <- c("(0,0)", "(0,1)","(1,0)", "(1,1)")
      barplot(c(y00, y01, y10, y11), horiz = TRUE, col = alt.col,
              names.arg = labels, xpd = TRUE, main = main[[i]],
              xlim = c(0, min(100, 1.25*xmax)),
              xlab = "Percentage of Simulations")
    }
    else if (is.numeric(qi[1])) {
      y1 <- qi[, 3] + qi[, 4]
      y2 <- qi[, 2] + qi[, 4]
      contour(kde2d(y1, y2), xlab = "Pr(Y1 = 1)", 
              ylab = "Pr(Y2 = 1)", main = main[[i]], ...)
    }
  }
  par(op)
}
