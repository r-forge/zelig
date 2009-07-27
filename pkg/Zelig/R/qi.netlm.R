qi.netlm <- function(object, simpar, x, x1 = NULL, y = NULL) {
  k <- length(getcoef(object))
  coef <- simpar[,1:k]
  #alpha <- simpar[,(k+1):ncol(simpar)]
  ev <- coef %*% t(x)
  qi <- list(ev=ev)
  qi.name <- list(ev="Expected Values: E(Y|X)")
  if(!is.null(x1)){
    ev1 <- coef %*% t(x1)
    qi$fd <- ev1-ev
    qi.name$fd <-
      "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    #tmp.ev <- qi$tt.ev <- yvar - qi$ev
    #qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
    tmp.ev <- yvar - qi$ev
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
  }
  list(qi=qi, qi.name=qi.name)
}



