qi.BetaReg <- function(object, simpar, x, x1 = NULL, y = NULL) {
  k <- ncol(x)
  coef <- simpar[,1:k]
  phi <- simpar[,(k+1):ncol(simpar)]
  eta <- coef %*% t(x)
  ev <- exp(eta) / (1 + exp(eta))
  a <- ev * phi
  b <- phi - ev * phi
  pr <- matrix(NA, ncol = ncol(ev), nrow = nrow(ev))
  for (i in 1:ncol(pr))
    pr[,i] <- sapply(a[,i], rbeta, n = 1, shape2 = b[,i])
  qi <- list(ev = ev, pr = pr)
  qi.name <- list(ev = "Expected Values: E(Y|X)",
                  pr = "Predicted Values: Y|X")
  if(!is.null(x1)){
    eta1 <- coef %*% t(x1)
    ev1 <- exp(eta1) / (1 + exp(eta1))
    qi$fd <- ev1 - ev
    qi.name$fd <-
      "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    #tmp.ev <- qi$tt.ev <- yvar - qi$ev
    tmp.ev <- yvar - qi$ev
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    #qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
  }
  list(qi=qi, qi.name=qi.name)
}
