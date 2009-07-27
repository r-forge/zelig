## dbailey@wustl.edu
## modified by Gregor Gorjanc 2008-01-07
## modified by Ferdi  10/30/07
## modified by delia 09/22/08
################################

qi.mer <- function(object, simpar, x, x1 = NULL, y = NULL) {
  
  x <- as.data.frame(x)
  if (!is.null(x1))
    x1 <- as.data.frame(x1)
  
  
  ## original dataset
  D <- eval(object@call$data, envir = parent.frame())
  fml <- eval(object@call$formula)
  parsefml <- .getRandAndFixedTerms(fml)
  
  betas <- simpar[["betas"]]
  gammas <- simpar[["gammas"]]
  alpha <- simpar[["scale"]]
  
  
  fTermsNames <- colnames(model.matrix(parsefml$fixed, data = D))
                                        #cat(fTermsNames, "\n")
  
  fTerms <- x[,fTermsNames]
  rTerms <- list()
  for (i in 1:length(parsefml$random)){
    ## for now, intercept is always present
    tt <- terms(parsefml$random[[i]])
    attr(tt,"intercept") <- 1   
    rTermsNames <- colnames(model.matrix(tt,data=D))
    rTerms[[i]] <- x[, rTermsNames]
  }
  names(rTerms) <- names(parsefml$random)
  if (!is.null(x1)){
    fTermsNames <- colnames(model.matrix(parsefml$fixed, data = D))
    fTerms.x1 <- x1[,fTermsNames]
    rTerms.x1 <- list()
    for (i in 1:length(parsefml$random)){
      tt <- terms(parsefml$random[[i]])
      attr(tt,"intercept") <- 1   
      rTermsNames <- colnames(model.matrix(tt,data=D))
      rTerms.x1[[i]] <- x1[, rTermsNames]
      ##rTermsNames <- colnames(model.matrix(parsefml$random[[i]],data=D))
      ##rTerms.x1[[i]] <- x1[, rTermsNames]
    }
    names(rTerms.x1) <- names(parsefml$random)
  }
        
  ## Expected Values and Predicted Values    
  if (class(object) == "mer"){


    family <- try(object@nlmodel$family$family ,silent=TRUE)
    if (inherits(family,"try-error")) {
        family=NULL
    }

    link <- try(object@nlmodel$family$link ,silent=TRUE)
    if (inherits(link,"try-error")) {
      link=NULL
    } 
    
    eta <- betas %*% t(as.matrix(fTerms))
    mu <- eta
    ## For predicted values, add in random effects draws
    for (i in 1:length(rTerms)){
      mu <- mu + gammas[[names(rTerms[i])]] %*% t(as.matrix(rTerms[[i]]))
    }
    if (is.null(family)){
      ev <- eta
      n <- length(mu[,1])
      pr <- matrix(NA, nrow=nrow(mu), ncol=ncol(mu))
      for (i in 1:ncol(mu)){
        pr[,i] <- rnorm(n, mean=mu[,i], sd=alpha)
      }
    } else {
      theta <- matrix(object@nlmodel$family$linkinv(eta), nrow=nrow(betas))
      mut <- matrix(object@nlmodel$family$linkinv(mu), nrow=nrow(betas))
      ev <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))
      pr <- matrix(NA, nrow=nrow(mut), ncol=ncol(mut))
      dimnames(ev) <- dimnames(pr) <- dimnames(theta)
      n <- length(mut[, 1])
      if (family == "binomial"){
        ev <- theta
        for (i in 1:ncol(mut)){
          pr[,i] <- as.character(rbinom(n, 1, mut[,i]))
        }
        if (!is.null(y)) {
          if (NCOL(y) > 1) {
            y <- y[,1]
          }
        }
      }
      else if (family == "Gamma"){
        ev <- theta * 1/alpha
        n <- length(mut[i,])
        for (i in 1:nrow(mut)){
          pr[i,] <- rgamma(n, shape = mut[i,], scale= 1/alpha)
        }
      }
      else if (family == "gaussian"){
        ev <- theta
        if (link == "log"){
          for (i in 1:ncol(mut)){
            pr[,i] <- rlnorm(n, meanlog=mut[,i], sdlog=alpha)
          }
        } else {
          stop(sprintf("no method for %s family and %s link", family, link))
        }
      }
      else if (family == "poisson"){
        ev <- theta
        for (i in 1:ncol(mut)){
          pr[,i] <- rpois(n, lambda = mut[,i])
        }
      } 
      else {
        stop(sprintf("no method for %s family", family))
      }
    }
  }
  qi <- list(ev=ev, pr=pr)
  qi.name <- list(ev="Expected Values: E(Y|X)", pr="Predicted Values: Y|X")
  if (!is.null(x1)){
    if (class(object) == "mer"){
      if (is.null(family)){
        ev1 <- betas %*% t(as.matrix(fTerms.x1))
      }
      else {
        theta1 <- 
          matrix(object@nlmodel$family$linkinv(betas %*% t(as.matrix(fTerms.x1))),
                 nrow = nrow(betas))
        if (family == "Gamma") {
          ev1 <- theta1 * 1/alpha
        }
        else {
          ev1 <- theta1
        }
        if (family == "binomial") {
          qi$rr <- ev1/ev
          qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
        }
      }
      qi$fd <- ev1-ev
      qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)" 
    }
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    if (!is.null(family)){
      if (family == "binomial"){ 
        tmp.pr <- yvar - as.integer(qi$pr)
      } else {
        tmp.pr <- yvar - qi$pr
      }
    }
    else {
      tmp.pr <- yvar - qi$pr
    }
    qi$att.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi$att.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
    qi.name$att.ev <- "Average Treatment Effect for the Treated: Y - EV"
    qi.name$att.pr <- "Average Treatment Effect for the Treated: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}

