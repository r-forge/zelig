callparamseiestim <- function(formula,data, covar = NULL,
                              const = 0.001, parSeed = NULL, ...){
  

  tmp <- cmei(formula=formula, data=data, covar=covar)
  data <- tmp$data
  nR<-tmp$nR
  nC <- tmp$nC

 if(!(is.null(covar))){
  covar<-tmp$covar
}

  mf <- match.call()
  t <- terms(mf$formula)
  out <- paramsei.estim(data=data, covar=covar, nR=nR, nC=nC, const=const, 
                        parSeed=parSeed)
  out$terms <- t

  return (out)
}

# CALL.DIFP
# Calculates penalty for given parameters
# p     - parameter vector R x (C-1)
# mx    - Column marginals
# my    - row marginals
# nR    - number of rows
# nC    - number of columns
# nP    - number of precincts
# const - weight for penalty
# covar

call.difp <- function(par, mx, my, covar, nR, nC, nP, const){
  g <- par[1:(nR*(nC-1))]
  if(is.numeric(covar)) {  
    d <- par[(nR*(nC-1)+1):(2*nR*(nC-1))]
    gamma <- array(0, dim = c(nR, nC-1, nP))
    diff <- 0
    for (i in 1:nP) {
      gamma[,,i] <- matrix(g + covar[i]*d, nrow = nR, ncol = nC-1, byrow = TRUE)
      expo <- exp(gamma[,,i]) 
      if (nC != 2) 
        ebeta <- exp(gamma[,,i]) / (1 + apply(exp(gamma[,,i]), 1, sum))
      else 
        ebeta <- exp(gamma[,,i]) / (1 + exp(gamma[,,i]))
      yhat <- mx[i,] %*% ebeta
      diff <- diff + sum((yhat - my[i, -nC])^2) + (const*sum(gamma[,,i]^2))
    }
  }
  else {
    d <- array(0, dim = nR * (nC-1))
    gamma <- matrix(g, nrow = nR, ncol = nC-1, byrow = TRUE)
    ebeta <- exp(gamma)/(1 + apply(exp(gamma), 1, sum))
    diff <- sum((mx %*% ebeta - my[, -nC])^2) + (const * sum(gamma^2))
  }

  ## Trap bad values
  if (is.null(diff))
    diff <- 9999999;
  if (!is.finite(diff))
    diff <- 9999999;
  
  diff
}


# Penalized Least Square Minimizer
# PARAMS.ESTIM
# Estimates parameters minimizing the penalized least squares criterion
# x       - index (optional, for bootstrapping)
# data    - marginals (optionally with covariates)
# nR      - number of rows
# nC      - number of columns
# const   - weight for penalty
# parSeed - Seed for parameters (optional)

paramsei.estim <- function(data, covar=NULL, nR, nC, const=0.001, 
                           parSeed=NULL) {
  
  normalizedata <- function(dt){
    s <- apply(dt,1,sum)
    return (dt/s) 
  }

  mc <- match.call(expand.dots = TRUE)
  #print(colnames(data))

  socialnames <- colnames(data[,1:nR])
  mx <- as.matrix(data[,1:nR])
  mx <- normalizedata(mx)
  for (i in 1:nrow(mx)){
    if (round(sum(mx[i,]), digits=4) != 1){
      stop ("The Sum of each row marginal (\"social classes\") should be 1. It seems like you have a problem in the row nr. ", i)
    }
  }
  partiesnames <- colnames(data[,(nR+1):(nR+nC)])
  my <- as.matrix(data[,(nR+1):(nR+nC)])
  my <- normalizedata(my)
  for(i in 1:nrow(my)){
    if (round(sum(my[i,]),digits=4)!= 1){
      stop ("The Sum of each column marginal (\"parties\") should be 1. It seems like you have a problem in the row nr. ", i)
    }
  }
  nP <- nrow(data)

  coef.names<-c()
  for(i in 1:(nR)){
    for(j in 1:(nC-1)){
      coef.names <- c(coef.names, paste(socialnames[[i]], ".",
                                        partiesnames[[j]], sep = ""))
    }
  }
  if(!is.null(covar)){
    if(is.null(parSeed)) parSeed = rnorm(2*nR*(nC-1))
    for (i in 1:(nR*(nC-1)))
      coef.names<-c(coef.names,paste("delta",i,sep=""))
    names(parSeed)<-coef.names
  }
  else {
    if(is.null(parSeed))  parSeed = rnorm(nR*(nC-1))
    names(parSeed)<-coef.names
  }

  fit <- optim(parSeed, fn = call.difp, method="L-BFGS-B", hessian=TRUE,
               covar = covar, nR = nR, nC = nC, nP = nP, mx = mx,
               my = my, const = const)
  fit$coefficients <- fit$par
  fit$cov1<-diag(1/diag(fit$hessian))
  fit$vcov <- solve(fit$hessian)
  fit$hessian<-fit$hessian
  fit$terms <- attr(all.vars, "terms")
  fit$call <- mc
  fit$contrasts <- attr(all.vars, "contrasts")
  fit$xlevels <- attr(all.vars, "xlev")
  fit$levels <- attr(all.vars, "lev")
  fit$dims <- c(nR, nC, nP)
  fit$covar <- covar
  fit$socialnames <- socialnames
  fit$partiesnames <- partiesnames
  class(fit) <- "eiRxC"
  return(fit)
}


# Calculate Fractions
# CALC.FRACTIONS
# Calculate fractions from the parameters
# p     - parameters
# nR    - number of rows
# nC    - number of columns
# covar - (Optional) Vector of covariates

calc.fractions <- function(object, simpar) {
  nR <- object$dims[1]
  nC <- object$dims[2]
  d <- array(0, dim = nR*(nC-1))
  if (!is.null(object$covar)) {
    covar<-as.matrix(object$covar)
    nP <- nrow(covar)
    ests <- array(0, dim = c(nR, nC, nP),
                  dimnames=list(object$socialnames,
                    object$partiesnames,
                    c(1:nP)))
    d <-object$coefficients[(nR*(nC-1)+1):(2*nR*(nC-1))]
    for(i in 1:nP) {
      estsTmp<- array(0, dim = c(nR, nC, nrow(simpar)))
      for(j in 1:nrow(simpar)){
        g <- simpar[j,1:(nR*(nC-1))]
        p.exp <- exp(g + d*covar[i])
        p.matrix <- matrix(p.exp, nrow = nR, byrow = TRUE)
   
        p.sums <- apply(p.matrix, 1, sum)
        p.sums <- p.sums + 1
        p.less <- p.matrix/p.sums
        estsTmp[,,j] <- cbind(p.less, 1 - apply(p.less, 1, sum))
      }
      ests[,,i]<-apply(estsTmp,c(1,2),mean)
    } 
  }
  else {
    ests <- array(0, dim = c(nR, nC, nrow(simpar)),
                  dimnames=list(object$socialnames,
                    object$partiesnames,
                    c(1:nrow(simpar))))
    for(i in 1:nrow(simpar)){
      g<-simpar[i,1:(nR*(nC-1))]
      p.matrix <- matrix(exp(g), nrow = nR, byrow = TRUE)
      p.sums <- apply(p.matrix, 1, sum)
      p.sums <- p.sums + 1
      p.less <- p.matrix / p.sums
      ests[,,i] <- cbind(p.less, 1 - apply(p.less, 1, sum))
    }
  }
  return (ests)
}


cmei <- function(formula, data, covar = NULL, ...){
  if (is.null(rownames(data))) {
    rownames(data) <- 1:nrow(data)
    assign(data, as.character(data), env = .GlobalEnv)
  }
  res<-NULL
  myVars<-all.vars(formula[[2]])
  mxVars<-all.vars(formula[[3]])
  allVars<-c(mxVars,myVars)
  nR<-length(mxVars)
  nC <-length(myVars)
  if(!(is.null(covar))){
    covar <- model.frame(covar, data)
    res$covar<-as.matrix(covar)
  }
  else
    res$covar<-NULL
  res$data<-data[,allVars]
  res$nR<-nR
  res$nC<-nC
  return(res)
}
