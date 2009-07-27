qi.gee <- function(object, simpar, x, x1 = NULL, y = NULL) {
  check <- FALSE
  model <- object$zelig
  k <- length(object$coef)
  coef <- simpar[,1:k]
  if (k < ncol(simpar)) 
    alpha <- simpar[,(k+1):ncol(simpar)]
  eta <- coef %*% t(x)
  theta <- matrix(object$family$linkinv(eta), nrow = nrow(coef))
  pr <- ev <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))
  dimnames(pr) <- dimnames(ev) <- dimnames(theta)
  if (model =="logit.gee") {
    check <- TRUE
    ev <- theta
    for (i in 1:ncol(theta)) 
      pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i]))
  }
  else if (model == "normal.gee") {
    ev <- theta
    for (i in 1:nrow(ev)) 
      pr[i,] <- rnorm(length(ev[i,]), mean = ev[i,], sd = alpha[i])
  }
  else if (model == "gamma.gee") {
    ev <- theta * 1/alpha
    for (i in 1:nrow(ev))  
      pr[i,] <- rgamma(length(ev[i,]), shape = theta[i,], scale = 1/alpha[i])
  }
  else if (model == "poisson.gee") {
    ev <- theta
    for (i in 1:ncol(ev))
      pr[,i] <- rpois(length(ev[,i]), lambda = ev[,i])
  }
  qi <- list(ev = ev, pr = pr)
  qi.name <- list(ev = "Expected Values: E(Y|X)",
                  pr = "Predicted Values: Y|X")
  if (!is.null(x1)){
    theta1 <- matrix(object$family$linkinv(coef %*% t(as.matrix(x1))),
                     nrow = nrow(coef))
    if (model == "gamma")
      ev1 <- theta1 * 1/alpha
    else
      ev1 <- theta1
    qi$fd <- ev1-ev
    qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
    if (model %in% c("logit", "probit", "relogit")) {
      qi$rr <- ev1/ev
      qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
    }
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    if (check)
      tmp.pr <- yvar - as.integer(qi$pr)
    else
      tmp.pr <- yvar - qi$pr
    qi$ate.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi$ate.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}






