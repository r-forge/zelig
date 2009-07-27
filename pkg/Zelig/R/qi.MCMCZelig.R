qi.MCMCZelig <- function(object, simpar=NULL, x, x1 = NULL, y = NULL, ...) {
  model <- getzelig(object)
  qi <- list()
  check <- FALSE
  if (model %in% c("logit.bayes", "probit.bayes", "oprobit.bayes", "mlogit.bayes")) 
    check <- TRUE
  if (model %in% c("logit.bayes","probit.bayes", "normal.bayes",
                   "poisson.bayes","tobit.bayes")) {
    if (model == "logit.bayes") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- 1/(1+exp(-eta))
      for (i in 1:ncol(ev)) 
        pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i])) 
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr = "Predicted Values: Y|X")
    }
    else if (model == "probit.bayes") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- pnorm(eta)
      for (i in 1:ncol(ev)) 
        pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i]))
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr = "Predicted Values: Y|X")
    }
    else if (model =="normal.bayes") {
      nvar <- ncol(object$coefficients) 
      coef <- object$coefficients[,1:(nvar-1)]
      qi$ev <- ev <- coef %*% t(x)
      qi$pr <- rnorm(nrow(qi$ev), qi$ev,
  sqrt(object$coefficients[,nvar]))
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr = "Predicted Values:Y|X")
      
    }
    else if (model =="tobit.bayes") {
      coef <- object$coefficients[,1:(ncol(object$coefficients)-1)]
      sig2 <- object$coefficients[,ncol(object$coefficients)]
      sig <- sqrt(sig2)
      eta <- coef %*% t(x)
      ev <- cev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(cev) <- dimnames(ev) <- dimnames(eta)
      L2 <- (object$above-eta)/sig
      L1 <- (object$below-eta)/sig
      ##cev <- eta + sig*(dnorm(L1)-dnorm(L2))/(pnorm(L2)-pnorm(L1))
      temp1 <- pnorm(L1)*object$below
      if (object$below==-Inf) temp1<-0

      temp2 <- (1-pnorm(L2))*object$above
      if (object$above==Inf) temp2<-0

      qi$ev <-ev <- temp1+eta*(pnorm(L2)-pnorm(L1))+sig*(dnorm(L1)-dnorm(L2))+temp2
      qi.name <- list(ev = "Expected Values: E(Y|X)")
    }
    else if (model == "poisson.bayes") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- exp(eta)
      for (i in 1:ncol(ev)) 
        pr[,i] <- rpois(length(ev[,i]), ev[,i])
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)",
                      pr="Predicted Values: Y|X")
    }
    if (!is.null(x1)) {
      eta1 <- coef %*% t(x1)
      if (model == "logit.bayes") {
        ev1 <- 1/(1+exp(-eta1))
        qi$fd <- ev1 - ev
        qi$rr <- ev1 / ev
        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
        qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
      }
      else if (model == "probit.bayes") {
        ev1 <- pnorm(eta1)
        qi$rr <-ev1/ev
        qi$fd <-ev1-ev
        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
        qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
      }
      else if (model == "normal.bayes") {
        ev1 <- eta1
        qi$fd <- ev1 - ev
        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
      }
      else if (model == "tobit.bayes") {
        L2 <- (object$above-eta1)/sig
        L1 <- (object$below-eta1)/sig
        ##cev <- eta + sig*(dnorm(L1)-dnorm(L2))/(pnorm(L2)-pnorm(L1))
        temp1 <- pnorm(L1)*object$below
        if (object$below==-Inf) temp1<-0

        temp2 <- (1-pnorm(L2))*object$above
        if (object$above==Inf) temp2<-0
        
        ev1 <- temp1+eta*(pnorm(L2)-pnorm(L1))+sig*(dnorm(L1)-dnorm(L2))+temp2
        qi$fd <-ev1-ev
        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
      }        
      else if (model == "poisson.bayes") {
        ev1 <- exp(eta1)
        qi$fd <- exp(eta1) - ev
        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
      }
    }
    if (!is.null(y)) {
      yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow
    = TRUE)
      tmp.ev <- yvar - qi$ev
      if (check) 
        tmp.pr <- yvar - as.integer(qi$pr)
      else
        tmp.pr <- yvar - qi$pr
      qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
      qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
      if (model %in% c("logit", "probit", "poisson")) {
        qi$att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
        qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
      }
    }
    out <- list(qi=qi, qi.name=qi.name)
  }
  else if ((model =="oprobit.bayes") || (model == "mlogit.bayes")) {
    if (model == "oprobit.bayes") {
      library(stats)
      p <- dim(model.matrix(object, data=eval(object$data)))[2]
      coef <- object$coefficients[,1:p]
      eta <- coef %*% t(x)
      level <- ncol(object$coefficients)-p+2
      gamma<-matrix(NA, nrow(object$coefficients), level+1) 
      gamma[,1] <- rep(-Inf, nrow(gamma))
      gamma[,2] <- rep(0, nrow(gamma))
      gamma[,ncol(gamma)]<-rep(Inf, nrow(gamma))
      if (ncol(gamma)>3)
        gamma[,3:(ncol(gamma)-1)] <-
          object$coefficients[,(p+1):ncol(object$coefficients)]
      ev <- array(NA, c(nrow(eta), level, ncol(eta)))
      pr <- matrix(NA, nrow(eta), ncol(eta))
      ##      dimnames(pr)[1] <- dimnames(ev)[1] <- dimnames(eta)[1]
      ##      dimnames(pr)[2] <- dimnames(ev)[3] <- dimnames(eta)[2]
      for (j in 1:level)
        ev[,j,] <- pnorm(gamma[,j+1]-eta) - pnorm(gamma[,j]-eta)
      colnames(ev) <- levels(model.response(model.frame(object)))
      for (j in 1:nrow(pr)) {
        mu <- eta[j,]
        ##       pr[j,]<-as.character(cut(mu, gamma[j,],
        ##       labels=as.factor(1:level)))
        pr[j,]<-as.character(cut(mu, gamma[j,], labels=colnames(ev)))   
      }
      colnames(ev) <- levels(model.response(model.frame(object)))
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: P(Y=j|X)",
                      pr="Predicted Values: Y|X")      
    }
    else if (model == "mlogit.bayes") {
      library(stats)
      resp <- model.response(model.frame(object))
      level <- length(table(resp))
      p <- dim(model.matrix(eval(object),data=eval(object$data)))[2]
      coef <- object$coefficients
      eta <- array(NA, c(nrow(coef),level, nrow(x)))
      eta[,1,]<-matrix(0, dim(eta)[1],dim(eta)[3])
      for (j in 2:level) {
        ind <- (1:p)*(level-1)-(level-j)
        eta[,j,]<- coef[,ind]%*%t(x)
      }
      eta<-exp(eta)
      ev <- array(NA, c(nrow(coef), level, nrow(x)))
      pr <- matrix(NA, nrow(coef), nrow(x))
      colnames(ev) <- rep(NA, level)
      for (k in 1:nrow(x)) {
        for (j in 1:level)
          ev[,j,k] <- eta[,j,k]/rowSums(eta[,,k])
      }
      for (j in 1:level) {
        colnames(ev)[j] <- paste("P(Y=", j, ")", sep="")
      }
      for (k in 1:nrow(x)) {             
        probs <- as.matrix(ev[,,k])
        temp <- apply(probs, 1, FUN=rmultinom, n=1, size=1)
        temp <- as.matrix(t(temp)%*%(1:nrow(temp)))
        pr <- apply(temp,2,as.character)
      }
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: P(Y=j|X)",
                      pr = "Predicted Values: Y|X")      
    }
    if (!is.null(x1)) {
      if (model == "oprobit.bayes") {
        eta1 <- coef %*% t(x1)
        ev1 <- array(NA, c(nrow(eta), level, ncol(eta)))
        for (j in 1:level)
          ev1[,j,] <- pnorm(gamma[,j+1]-eta1) - pnorm(gamma[,j]-eta1)
        qi$rr <-ev1/ev
        qi$fd <-ev1-ev
        qi.name$fd <- "First Differences in Expected Values: P(Y=j|X1)-P(Y=j|X)"
        qi.name$rr <- "Risk Ratios: P(Y=j|X1)/P(Y=j|X)"
      }
      else if (model == "mlogit.bayes") {
        eta1 <- array(NA, c(nrow(coef),level, nrow(x1)))
        eta1[,1,]<-matrix(0, dim(eta1)[1],dim(eta1)[3])
        for (j in 2:level) {
          ind <- (1:p)*(level-1)-(level-j)
          eta1[,j,]<- coef[,ind]%*%t(x1)
        }
        eta1<-exp(eta1)
        ev1 <- array(NA, c(nrow(eta1), level, nrow(x1)))
        for (k in 1:nrow(x)) {
          for (j in 1:level)
            ev1[,j,k] <- eta1[,j,k]/rowSums(eta1[,,k])
        }
        qi$rr <-ev1/ev
        qi$fd <-ev1-ev
        qi.name$fd <- "First Differences in Expected Values: P(Y=j|X1)-P(Y=j|X)"
        qi.name$rr <- "Risk Ratios: P(Y=j|X1)/P(Y=j|X)"
      }
    }
    if (!is.null(y)) {
      yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
      levels.names<-levels(as.factor(y))
      yvar1<- pr1 <-array(NA, c(nrow(yvar), level, ncol(yvar)))
      for (j in 1:nrow(yvar)) {
        yvar1[j,,]<-t(class.ind(yvar[j,], levels.names))
        if (check)
          pr1[j,,]<-t(class.ind(as.integer(qi$pr[j,]), levels.names))
        else
          pr1[j,,]<-t(class.ind(qi$pr[j,],levels.names))
      }
      tmp.ev <- yvar1 - qi$ev
      tmp.pr <- yvar1 - pr1
      qi$att.ev <- matrix(apply(tmp.ev, 2, rowMeans), nrow = nrow(simpar))
      qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
      if (model %in% c("oprobit.bayes", "mlogit.bayes", "normal.bayes")) {
        qi$att.pr <- matrix(apply(tmp.pr, 2, rowMeans), nrow = nrow(simpar))
        qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
      }
    }
    out <- list(qi=qi, qi.name=qi.name) 
  }
  else if (model == "ei.hier" || model == "ei.dynamic") {
    if (!any(class(x)=="cond")) stop("set 'cond=TRUE' in setx.\n")
    else {
      coef <- object$coefficients
      n <- nrow(x)
      if (is.null(object$N))
        N<-rep(1,n)
      else N <- eval(object$N)
      ev <- array(NA, c(nrow = nrow(coef), 2,2, n))
      pr <- array(NA, c(nrow = nrow(coef), 2,2, n))
      nlen<-length(coef[,1])
      for (j in 1:2) {
        ev[,j,1,] <- t(apply(coef[,((1:n)+(j-1)*n)], 1,"*",  x[,j])*N)
        ev[,j,2,] <- t(apply((1-coef[,((1:n)+(j-1)*n)]), 1,"*", x[,j])*N)
        for (i in 1:n) {
          size<-round(x[i,j]*N[i])
          pr[,j,1,i] <-rbinom(prob=coef[,(i+(j-1)*n)],  n=nlen, size=size)
          pr[,j,2,i] <- x[i,j]*N[i]-pr[,j,1,i]
        }
      }
      ##        dimnames(ev)[[1]] <- dimnames(pr)[[4]] <- 1:nrow(coef)
      dimnames(ev)[[4]] <- dimnames(pr)[[4]] <- rownames(x)
      dimnames(ev)[[2]] <- dimnames(pr)[[2]] <- colnames(x)
      dimnames(ev)[[3]] <- dimnames(pr)[[3]] <- colnames(model.response(object$model))
      class(ev) <- class(pr) <- c("ei", "array")    
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected In sample predictions at aggregate level",
                      pr = "In sample predictions at aggregate level")
    }
    out <- list(qi=qi, qi.name=qi.name)
  }
  else if ( model %in% c("factor.bayes", "factor.ord", "factor.mix", "irt1d", "irtkd")) {
    stop("sim procedure not applicable since no explanatory variables are involved.\n")
    out <- list(qi=qi)
  }
  out
}  
  
  



