qi.gee <- function(object, simpar, x, x1 = NULL, y = NULL) {
  model <- getzelig(object)
  coef <- simpar
  eta <- coef %*% t(x)
  ev <- theta <- matrix(object$family$linkinv(eta), nrow = nrow(coef)) 
  qi <- list(ev = ev)
  qi.name <- list(ev = "Expected Values: E(Y|X)")
  if (!is.null(x1)){
    ev1 <- theta1 <- matrix(object$family$linkinv(coef %*% t(as.matrix(x1))),
                     nrow = nrow(coef))
    qi$fd <- ev1-ev
    qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
    if (model %in% c("logit.gee", "probit.gee")) {
      qi$rr <- ev1/ev
      qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
    }
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
#    tmp.ev <- qi$tt.ev <- yvar - qi$ev
#    qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
  }
  list(qi=qi, qi.name=qi.name)
}
