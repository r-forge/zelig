qi.polr <- function(object, simpar, x = x, x1 = NULL) {
  m <- ncol(x)
  lev <- object$lev
  k <- length(lev)
  ev.multinom<-function(num, sims, x, lev, k, m){
    eta <- NULL
    for (i in 1:(k-1))
      eta <- cbind(eta, sims[,(m*(i-1)+1):(m*i)]%*%t(x))
    eta <- exp(cbind(rep(0, num), eta))
    ev <- eta/apply(eta, 1, sum)
    colnames(ev) <- lev
    ev
  }
  ev <- ev.multinom(num, simpar, x, lev, k, m)
  Ipr <- sim.cut <- matrix(NA, nrow = nrow(ev), ncol = ncol(ev))
  colnames(Ipr) <- colnames(sim.cut) <- lev
  sim.cut[,1] <- ev[,1]
  for (j in 2:k) 
    sim.cut[,j] <- sim.cut[,(j-1)] + ev[,j]
  tmp <- runif(nrow(ev), 0, 1)
  for (l in 1:k) 
    Ipr[,l] <- tmp > sim.cut[,l]
  pr <- NULL
  for (m in 1:nrow(Ipr))
    pr[m] <- 1 + sum(Ipr[m,])
  res$qi <- list(ev=ev, pr=pr)
  res$qi.name <- list(ev = "Predicted Probabilities: P(Y=j|X)",
                      pr = "Predicted Values: Y|X")
  if(!is.null(x1)){
    ev1 <- ev.multinom(num, simpar, x1, lev, k, m)
    res$qi$fd <- ev1-ev
    res$qi.name$fd <- "First Differences: P(Y=j|X1)-P(Y=j|X)"
  }
  list(qi=qi, qi.name=qi.name)
}
