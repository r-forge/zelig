plot.zelig.oprobit <- function(x, xlab = "", user.par = FALSE, alt.col = NULL, ...){
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par)
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  if (!is.null(x$qi$rr)) {
    k <- k - 1
    x$qi$rr <- x$qi.name$rr <- NULL
  }
  par(mfrow = c(k,1))
  pr <- x$qi$pr
  spr <- array()
  lev <- sort(unique(pr))
  K <- length(lev)
  if (is.null(alt.col))
    alt.col <- rainbow(K)
  total <- length(pr)
  for (i in 1:K)
    spr[i] <- 100 * sum(as.character(pr) == lev[i])/total
  xmax <- max(spr)
  labels <- paste("Y=", lev, sep = "")
  barplot(spr, horiz = TRUE, col = alt.col, names.arg = labels,
          las = 1, main = x$qi.name$pr,
          xlim = c(0, min(100, 1.15*xmax)),
          xlab = "Percentage of Simulations")
  x$qi$pr <- x$qi.name$pr <- NULL
  main <- x$qi.name
  for (i in 1:(k-1)) {
    qi <- x$qi[[i]]
    if (length(dim(qi)) == 3 && dim(qi)[3] == 1)
      qi <- qi[,,1]
    dens <- list()
    xmax <- ymax <- array()
    for (j in 1:ncol(qi)) {
      dens[[j]] <- density(qi[,j])
      xmax[j] <- max(dens[[j]]$x)
      ymax[j] <- max(dens[[j]]$y)
    }
    plot(dens[[1]], col = alt.col[1],
         xlim = c(min(min(qi), 0), max(xmax)),
         xlab = xlab, main = "",
         ylim = c(0, max(ymax)), ...)
    for (j in 2:ncol(qi)) 
      lines(dens[[j]], col = alt.col[j])
    title(main = x$qi.name[[i]][1])
  }
  par(op)
}
