qi.gam <- function(object, simpar, x, x1 = NULL, y = NULL, num = NULL) {
num=nrow(simpar)
x <- as.data.frame(x)
if(!is.null(x1)){x1 <- as.data.frame(x1)}
  check <- FALSE
  model <- getzelig(object)
  k <- length(object$coef)
  coef <- simpar[,1:k]
  if (k < ncol(simpar)) 
    alpha <- simpar[,(k+1):ncol(simpar)]
  eta <- coef %*% t(x)				## Here is the nonconformability problem
  theta <- matrix(object$family$linkinv(eta), nrow = nrow(coef))
  pr <- ev <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta)) ## 1000 by 1 matrix
  dimnames(pr) <- dimnames(ev) <- dimnames(theta)
  if (model %in% c("logit.gam", "probit.gam")) { 
    check <- TRUE
	evfit <- predict.gam(object, x, se.fit=TRUE, type="link")$fit
	evse <-  predict.gam(object, x, se.fit=TRUE, type="link")$se.fit
	ev <- rnorm(num, mean=object$family$linkinv(evfit), sd=evse)
	prfit <- predict.gam(object, x, se.fit=TRUE, type="response")$fit 
	pr <- rbinom(num, 1, prfit)
    if (!is.null(y)) {
      if (NCOL(y) > 1)
        y <- y[,1]
    }
  }
  else if (model == "normal.gam") {
	evfit <- predict.gam(object, x, se.fit=TRUE, type="link")$fit
	evse <-  predict.gam(object, x, se.fit=TRUE, type="link")$se.fit
	ev <- rnorm(num, mean=object$family$linkinv(evfit), sd=evse)
	prfit <- predict.gam(object, x, se.fit=TRUE, type="response")$fit 
	prse <- predict.gam(object, x, se.fit=TRUE, type="response")$se.fit 
	pr <- rnorm(num, mean=prfit, sd=prse)
  }
  else if (model == "poisson.gam") {
	evfit <- predict.gam(object, x, se.fit=TRUE, type="link")$fit
	evse <-  predict.gam(object, x, se.fit=TRUE, type="link")$se.fit
	ev <- rnorm(num, mean=object$family$linkinv(evfit), sd=evse)
	prfit <- predict.gam(object, x, se.fit=TRUE, type="response")$fit 
	pr <- rpois(num, prfit)
  }
  qi <- list(ev = ev, pr = pr)
  qi.name <- list(ev = "Expected Values: E(Y|X)",
                  pr = "Predicted Values: Y|X")
  if (!is.null(x1)){
	evfit1 <- predict.gam(object, x1, se.fit=TRUE, type="link")$fit
	evse1 <-  predict.gam(object, x1, se.fit=TRUE, type="link")$se.fit
	ev1 <- rnorm(num, mean=object$family$linkinv(evfit), sd=evse)
    qi$fd <- ev1-ev
    qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
    if (model %in% c("logit.gam", "probit.gam")) {
      qi$rr <- ev1/ev
      qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
    }
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
#    tmp.ev <- qi$tt.ev <- yvar - qi$ev
#    qi.name$tt.ev <- "Unit Treatment Effect for the Treated: Y - EV"
#   if (check)
#     tmp.pr <- qi$tt.pr <- yvar - as.integer(qi$pr)
#   else
#      tmp.pr <- qi$tt.pr <- yvar - qi$pr
#    qi.name$tt.pr <- "Unit Treatment Effect for the Treated: Y - PR"
    tmp.ev <- yvar - qi$ev
    if (check)
      tmp.pr <- yvar - as.integer(qi$pr)
    else
      tmp.pr <- yvar - qi$pr
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi$att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
    qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}






