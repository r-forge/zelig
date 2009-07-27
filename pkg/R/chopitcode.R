## Need to register the following in the NAMESPACE
##    S3method(coef, chopit)
##    S3method(vcov, chopit)
##    S3method(qi, chopit)

coef.chopit <- function(object, ...) {
  object$chopit.optim$par
}

vcov.chopit <- function(object, ...) {
  solve(object$chopit.hess)
} 

print.chopit <- function(x, ...) {
  cat("Use summary() for more information.\n")
}

zelig2chopit <- function(formula, model, data, M, ...) {
  require(anchors)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- NULL
  mf$M <- mf$robust <- NULL
  mf[[1]] <- prechop
  as.call(mf)
}

prechop <- function(formula, data, ...) {
  "%w/o%" <- function(x,y) x[!x %in% y]
  ## Setting up complete formula
  if (is.list(formula)) { 
    fs <- sapply(formula$self, deparse, width.cutoff = 500)
    fv <- sapply(formula$vign, deparse, width.cutoff = 500)
    ft <- sapply(formula$tau, deparse, width.cutoff = 500)
  } else { stop("formula must be a list of formula objects with names `self', `vign', and `tau'.") }
  s2 <- unlist(strsplit(fs[2], "cbind(", fixed = TRUE))
  s2 <- unlist(strsplit(s2, ",", fixed = TRUE))
  s2 <- unlist(strsplit(s2, ")", fixed = TRUE))
  v2 <- unlist(strsplit(fv[2], "cbind(", fixed = TRUE))
  v2 <- unlist(strsplit(v2, ",", fixed = TRUE))
  v2 <- unlist(strsplit(v2, ")", fixed = TRUE))
  xs <- all.vars(formula$self) %w/o% s2
  xt <- all.vars(formula$tau)
  xx <- unique(c(xt, xs))
  f1 <- as.formula(paste("~", paste(s2, collapse = "+"), "+",
                         paste(xs, collapse = "+")))
  f2a <- as.formula(paste("~", paste(xt, collapse = "+")))
  f2b <- as.formula(paste("~", paste(v2, collapse = "+")))
  f2c <- as.formula(paste("~", paste(v2, collapse = "+"), "+",
                          paste(xt, collapse = "+")))

  ## Setting up complete data frame
  if (is.list(data) & !is.data.frame(data)) {
    if (all( c("self", "vign") %in% names(data))) {
      DD <- model.frame(formula$self, data = data$self)
      D1 <- model.frame(f1, data = data$self)
      D2a <- rownames(model.frame(f2a, data = data$vign))
      D2b <- model.frame(f2b, data = data$vign, na.action = NULL)
    } else if (length(data) == 2){
      DD <- model.frame(as.formula(formula[[1]]), data = data[[1]])
      D1 <- model.frame(f1, data = data[[1]])
      D2a <- rownames(model.frame(f2a, data = data[[2]]))
      D2b <- model.frame(f2b, data = data[[2]], na.action = NULL)
    } else if (length(data) > 2) { stop("data cannot have more than two data frames.") } 
    ridx <- names(which(apply(!apply(D2b, 1, is.na), 2, all)))
    tmp <- data$vign[ridx %in% c(rownames(data$vign), D2a), c(names(D2b), xx)]
    D2 <- model.frame(f2c, data = tmp)
    remove(tmp)
    idx1 <- names(D1)
    idx2 <- names(D2)
    miss1 <- idx1[!(idx1 %in% idx2)]
    miss2 <- idx2[!(idx2 %in% idx1)]
    for (i in miss1) { D2[[i]] <- rep(NA, length = nrow(D2)) }
    for (i in miss2) { D1[[i]] <- rep(NA, length = nrow(D1)) } 
    data <- rbind(D1, D2)
  } else if (is.data.frame(data)) {
    D1 <- data <- data
  }
  else { stop("data must be either a data frame or list of data frames") } 
  out <- chopit(formula, data = data, ...)
  out$par <- out$chopit.optim$par
  out$value <- out$chopit.optim$value
  out$counts <- out$chopit.optim$counts
  out$convergence <- out$chopit.optim$convergence
  out$message <- out$chopit.optim$message
  out$formula <- list()
  out$formula$self <- as.formula(formula$self)
  out$formula$tau <- as.formula(formula$tau)
  out$formula$vign <- as.formula(formula$vign)
  out$data <- D1
  tt <- attr(D1, "terms")
  tt[[3]] <- all.vars(tt)[1]
  attr(tt, "factors") <-  attr(tt, "factors")[,-1]
  attr(tt, "response") <- 1
  attr(out, "terms") <- out$terms <- attr(out$data, "terms") <- tt
#  ttt <- attr(tt,"dataClasses")
#  ttt[ttt == "factor"] <- "character"
#  attr(tt, "dataClasses") <- ttt

  out
}

qi.chopit <- function(object, simpar, x, x1 = NULL, y = NULL) {

  getcut <- function(x) x[1]
  getpars <- function(x) x[2]
  
  getsimpars <- function(simpar, k, name) {
    idx <- match(name, names(k))
    if (idx == 1) { k1 <- 1
    } else k1 <- sum(k[1:(idx - 1)]) + 1
    k2 <- sum(k[1:idx])
    if ((k2 - k1) > 0) {
      return(simpar[, k1:k2])
    } else return(NULL)
  }

#  keeptau <- function(xx, yy, mu, sigma) {
#     out <- NULL
#    out[one] <- pnorm(xx[yy[one]], mean = mu[one], sd = sigma)
#    out[last] <- 1 - pnorm(xx[(yy[last]-1)], mean = mu[last], sd = sigma)
#    out[inter] <- pnorm(xx[yy[inter]], mean = mu[inter], sd = sigma) - 
#      pnorm(xx[(yy[inter]-1)], mean = mu[inter], sd = sigma)
#    out
#  }

  makepr <- function(mu, xx, nt, y, control) {
    tau <- xx[1:nt]
    sigma <- xx[nt+1]
    switch(as.character(control), "1" = pnorm((tau[1] - mu) / sigma),
           "2" = pnorm((tau[y] - mu) / sigma) - pnorm((tau[y-1] - mu) / sigma),
           "3" = 1 - pnorm((tau[nt] - mu) / sigma))
  }

  fn <- function(mu, xxx, nt, control, y, psd) {
    pm <- xxx[(nt + 2)]
    p1 <- makepr(mu = mu, xx = xxx[1:(nt+1)], nt = nt, control = control,
                 y = y)
    mu * p1 * dnorm((mu - pm) / psd)
  }

  fint <- function(xxx, nt, control, psd, y) {
    integrate(fn, lower = -Inf, upper = Inf,
              xxx = xxx, nt = nt, control = control,
              psd = psd, y = y)$value
  }
  
  num <- nrow(simpar)
  labs <- object$chopit.parm$labels
  ests <- object$chopit.parm$estimated
  for (i in names(labs)) labs[[i]] <- labs[[i]][ests[[i]]]
  k <- sapply(labs, length)
  colnames(simpar) <- unlist(labs)

  gpars <- getsimpars(simpar, k, name = "gamma")
  lnse.re <- getsimpars(simpar, k, name = "lnse.re")
  if (is.null(lnse.re)) { omega <- 0 } else { omega <- exp(lnse.re) }
  lnse.self <- getsimpars(simpar, k, name = "lnse.self")
  if (is.null(lnse.self)) { sigma <- 1 } else { sigma <- exp(lnse.self)}
  ## Only need lnse.vign = sigmaj for distn in footnote 8, p.209 (APSR)
  ##  lnse.vign <- getsimpars(simpar, k, name = "lnse.vign")
  ##  if (is.na(match("lnse.vign.vign1", colnames(lnse.vign)))) {
  ##    lnse.vign <- cbind(lnse.vign1 = rep(0, num), lnse.vign, rep(1, num))
  ##    colnames(lnse.vign)[ncol(lnse.vign)] <-
  ##      paste("lnse.vign.vign", ncol(lnse.vign), sep = "")
  ##  }
  ## sigmaj <- exp(lnse.vign)  
  ## theta1 <- getsimpars(simpar, k, name = "theta1")
  beta <- getsimpars(simpar, k, name = "beta")

  fmls <- object$formula
  if (length(names(fmls)) == 3) {
    ft <- fmls$tau
    fs <- fmls$self
  } else {
    ft <- fmls[[3]]
    fs <- fmls[[1]]
  }
  
  vid <- sort(ncol(vcov(object)) - (0:(length(labs$beta) - 1)))
  Vbeta <- vcov(object)[vid,vid]
  X <- as.matrix(x)[,-1, drop = FALSE]
  ev <- X %*% t(beta)
  qi <- list(ev = t(ev))
  qi.name <- list(ev = "Expected value: E(mu|x)")
  if (!is.null(x1)) {
    X1 <- as.matrix(x1)[,-1, drop = FALSE]
    ev1 <- X1 %*% t(beta)
    qi$fd <- t(ev1 - ev)
    qi.name$fd <- "First Differences in Expected Value: E(mu|x1) - E(mu|x)"
  }
  if (!is.null(y)) {
    pmean <- X %*% t(beta)
    pvar <- diag(X %*% Vbeta %*% t(X) + omega^2)
    tmp <- strsplit(unlist(strsplit(labs$gamma, "gamma1.", fixed = TRUE)),
                    ".", fixed = TRUE)
    cuts <- na.omit(unique(sapply(tmp, getcut)))
    pars <- na.omit(unique(sapply(tmp, getpars)))
    gamma <- array(NA, dim = c(num, length(pars), length(cuts)),
                    dimnames = list(NULL, pars, cuts))
    for (i in cuts) {
      tmp <- paste("gamma1", i, pars, sep = ".")
      idx <- match(tmp, labs$gamma)
      gamma[,,i] <- simpar[,idx]
    }
    
    check <- identical(deparse(ft[[length(ft)]], width.cutoff = 500),
                       deparse(fs[[3]], width.cutoff = 500))
    checkInt <- "(Intercept)" %in% pars
    if (check) {
      V <- X
    } else {
      xt <- all.vars(ft)
      xidx <- NULL
      for (ii in 1:length(xt)) xidx <- c(xidx, grep(xt[ii], colnames(X)))
      V <- X[, xidx, drop = FALSE]
    }
    if (checkInt) V <- cbind("(Intercept)" = rep(1, nrow(V)), V)
   
    tau <- array(NA, c(num, length(cuts), nrow(x)),
                 dimnames = list(NULL, cuts, rownames(x)))
    for (i in 1:num) tau[i,,] <- V %*% drop(gamma[i,,])
    tau[,2:ncol(tau),] <- exp(tau[,2:ncol(tau),])
    tau1 <- aperm(apply(tau, c(1,3), cumsum), c(2, 1, 3))

    control <- rep(2, length(y))
    control[which(y == 1)] <- 1
    control[which(y == (ncol(tau) + 1))] <- 3
    if (length(sigma) == 1) sigma <- rep(sigma, num)

    cev <- matrix(NA, nrow = num, ncol = nrow(X),
                 dimnames = list(NULL, rownames(X)))

    for (i in 1:length(y)) {
      if ((i %% 10) == 0)
        cat("Calculating E(mu|X,Y) for observation", i, "of", length(y), "\n")
      tmp <- cbind(tau[,,i], sigma, pmean[i,])
      cev[,i] <- apply(tmp, 1, fint, psd = sqrt(pvar[i]), 
                      control = as.character(control[i]), nt = dim(tau)[2],
                      y = y[i])
    }
    qi$cev <- cev
    qi.name$cev <- "Conditional Expected Value:  E(mu|X, Y)"
  }
  list(qi = qi, qi.name = qi.name)
}
