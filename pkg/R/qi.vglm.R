qi.vglm <- function (object, simpar, x, x1=NULL, y = NULL) {
  model <- getzelig(object)
  cm <- object@constraints
  if (model=="mlogit")
    ndim <- (ncol(object@y)-1)
  else if (model=="blogit" || model=="bprobit")
    ndim <- 3
  v <- rep(list(NULL), ndim)
  for(i in 1:length(cm)) {
    if(ncol(cm[[i]])==1){
      for(j in 1:ndim)
        if(sum(cm[[i]][j,])==1)
          v[[j]] <- c(v[[j]], names(cm)[i])
    }
    else {
      for (j in 1:ndim)
        if (sum(cm[[i]][j,])==1)
          v[[j]] <- c(v[[j]], paste(names(cm)[i], ":", j, sep=""))
    }
  }
  all.coef <- NULL
  for(i in 1:ndim)
    all.coef <- c(all.coef, list(simpar[,v[[i]]]))
  if (model=="mlogit"){
    if(is.null(colnames(object@y)))
      cnames <- ynames <- seq(1,ndim+1,1)
    else
      cnames <- ynames <- colnames(object@y)
    for(i in 1:(ndim+1))
      cnames[i] <- paste("Pr(Y=", ynames[i],")",sep="")
  }
  else if(model=="blogit" || model=="bprobit") 
    cnames <- c("Pr(Y1=0, Y2=0)", "Pr(Y1=0, Y2=1)",
                "Pr(Y1=1, Y2=0)", "Pr(Y1=1, Y2=1)")
  else
    stop(paste(model, "is not supported"))
  pp.vglm <- function(object, cm, all.coef, x, ndim, cnames){
    xm <- rep(list(NULL), ndim)
    for (i in 1:length(cm))
      for (j in 1:length(xm))
        if (sum(cm[[i]][j,]) == 1)
          xm[[j]] <- c(xm[[j]], x[,names(cm)[i]])
    sim.eta <- NULL
    for (i in 1:ndim)
      sim.eta <- cbind(sim.eta, all.coef[[i]] %*% as.matrix(xm[[i]]))
    ev <- object@family@inverse(sim.eta)
    colnames(ev) <- cnames
    ev
  }
  pr.vglm <- function(object, ev, ynames) { # To assign predicted values.
    model <- getzelig(object)
    if (model == "mlogit") {
      k <- ncol(ev)
      Ipr <- sim.cut <- matrix(NA, nrow = nrow(ev), ncol = ncol(ev))
      colnames(Ipr) <- colnames(sim.cut) <- colnames(ev)
      sim.cut[,1] <- ev[,1]
      for (j in 2:k) 
        sim.cut[,j] <- sim.cut[,(j-1)] + ev[,j]
      tmp <- runif(nrow(ev), 0, 1)
      for (l in 1:k) 
        Ipr[,l] <- tmp > sim.cut[,l]
      pr <- NULL
      for (m in 1:nrow(Ipr))
        pr[m] <- 1 + sum(Ipr[m,])
      pr <- factor(pr, levels = sort(unique(pr)), labels = ynames)
      if (model == "mlogit")
        pr <- factor(pr, ordered = FALSE)
      pr <- matrix(pr, nrow = dim(ev)[1])
    }
    else if (model == "blogit" || model == "bprobit") {
      mpr <- cbind((ev[,3] + ev[,4]), (ev[,2] + ev[,4]))
      index <- matrix(NA, ncol = 2, nrow = nrow(mpr))
      for (i in 1:2)
        index[,i] <- rbinom(length(mpr[,i]), 1, mpr[,i])
      pr <- matrix(NA, nrow(simpar), 4)
      pr[,1] <- as.integer(index[,1] == 0 & index[,2] == 0)
      pr[,2] <- as.integer(index[,1] == 0 & index[,2] == 1)
      pr[,3] <- as.integer(index[,1] == 1 & index[,2] == 0)
      pr[,4] <- as.integer(index[,1] == 1 & index[,2] == 1)
      colnames(pr) <- c("(Y1=0, Y2=0)", "(Y1=0, Y2=1)", "(Y1=1, Y2=0)",
                        "(Y1=1, Y2=1)")
    }
    pr
  }
  if (nrow(x) == 1) {
    ev <- pp.vglm(object, cm, all.coef, x, ndim, cnames)
    pr <- pr.vglm(object, ev, ynames)
  }
  else {
    ev <- array(dim = c(nrow(simpar), ndim+1, nrow(x)))
    if (model == "mlogit")
      pr <- matrix(nrow=nrow(simpar), ncol=nrow(x))
    else if (model == "blogit" || model == "bprobit")
      pr <- array(dim = c(nrow(simpar), 4, nrow(x)))
    for (i in 1:nrow(x)){
      tmp <- matrix(x[i,], nrow=1)
      colnames(tmp) <- colnames(x)
      rownames(tmp) <- rownames(x)[i]
      tmp.ev <- pp.vglm(object, cm, all.coef, tmp, ndim, cnames)
      tmp.pr <- pr.vglm(object, tmp.ev, ynames)
      ev[,,i] <- tmp.ev
      if (model == "blogit" || model == "bprobit")
        pr[,,i] <- tmp.pr
      else if (model == "mlogit")
        pr[,i] <- tmp.pr
    }
    dimnames(ev) <- list(rownames(tmp.ev), colnames(tmp.ev), NULL)
    if (model == "blogit" || model == "bprobit")
      dimnames(pr) <- list(rownames(tmp.pr), colnames(tmp.pr), NULL)
    else if (model == "mlogit")
      dimnames(pr) <- list(c(1:nrow(simpar)), NULL)
  }
  if (model=="mlogit") {
    qi <- list(ev=ev, pr=pr)
    qi.name <- list(ev="Predicted Probabilities: Pr(Y=k|X)",
                    pr="Predicted Values: Y=k|X")
  }
  else if (model=="blogit" || model=="bprobit") {
    qi <- list(ev=ev, pr=pr)
    qi.name <- list(ev="Predicted Probabilities: Pr(Y1=k,Y2=l|X)",
                    pr="Predicted Values: (Y1,Y2)|X")
  }
  if (!is.null(x1)) {
    if (nrow(x1) == 1)
      ev1 <- pp.vglm(object, cm, all.coef, x1, ndim, cnames)
    else {
      ev1 <- array(dim = c(nrow(simpar), ndim+1, nrow(x1)))
      for (i in 1:nrow(x1)) {
        tmp <- matrix(x1[i,], nrow=1)
        colnames(tmp) <- colnames(x1)
        rownames(tmp) <- rownames(x1)[i]
        tmp <- pp.vglm(object, cm, all.coef, tmp, ndim, cnames)
        ev1[,,i] <- tmp
      }
      dimnames(ev1) <- list(rownames(tmp), colnames(tmp), NULL)
    }
    qi$fd <- ev1 - ev
    if (model=="mlogit") {
      qi$rr <- ev1 / ev
      qi.name$fd <- "First Differences: Pr(Y=k|X1) - Pr(Y=k|X)"
      qi.name$rr <- "Risk Ratios: Pr(Y=k|X1) / Pr(Y=k|X)"
    }
    else if (model=="blogit" || model=="bprobit") {
      qi$rr <- ev1/ev
      qi.name$fd <- "First Differences: Pr(Y1=k,Y2=l|X1) - Pr(Y1=k,Y2=l|X)"
      qi.name$rr <- "Risk Ratios: Pr(Y1=k,Y2=l|X1) / Pr(Y1=k,Y2=l|X)"
    }
  }
  if (!is.null(y)) {
    tmp.ev <- tmp.pr <- array(NA, dim = dim(qi$ev))
    qi$att.ev <- qi$att.pr <- matrix(NA, dim(qi$ev)[1], dim(qi$ev)[2])
    if (model=="mlogit" || model=="oprobit") {
      yvar <- matrix(NA, nrow = length(y), ncol = length(ynames))
      pr.idx <- array(NA, dim = c(nrow(pr), length(ynames), nrow(x)))
      for (i in 1:length(ynames)) {
        yvar[,i] <- as.integer(y == ynames[i])
        pr.idx[,i,] <- as.integer(pr[,i] == ynames[i])
      }
      colnames(qi$att.ev) <- colnames(qi$att.pr) <- ynames
    }
    else if (model=="blogit" || model=="bprobit") {
      yvar <- matrix(NA, nrow = nrow(y), ncol = 4)
      yvar[,1] <- as.integer(y[,1] == 0 & y[,2] == 0)
      yvar[,2] <- as.integer(y[,1] == 0 & y[,2] == 1)
      yvar[,3] <- as.integer(y[,1] == 1 & y[,2] == 0)
      yvar[,4] <- as.integer(y[,1] == 1 & y[,2] == 1)
      pr.idx <- array(NA, dim = c(nrow(pr), 4, nrow(x)))
      for (i in 1:4)
        pr.idx[,i,] <- as.integer(pr[,i,])
      colnames(qi$att.ev) <- colnames(qi$att.pr) <-
        c("(Y1=0, Y2=0)", "(Y1=0, Y2=1)",
          "(Y1=1, Y2=0)", "(Y1=1, Y2=1)")
    }
    for (j in 1:ncol(yvar)) {
      for (i in 1:nrow(simpar)) {
        tmp.ev[i,j,] <- yvar[,j] - qi$ev[i,j,]
        tmp.pr[i,j,] <- yvar[,j] - pr.idx[i,j,]
      }
      qi$att.ev[,j] <- apply(tmp.ev[,j,], 1, mean)
      qi$att.pr[,j] <- apply(tmp.pr[,j,], 1, mean)
    }
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
    qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}
















