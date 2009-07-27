qi.rq <- function(object, simpar, x, x1=NULL, y=NULL) {
    tau <- object$tau 
    eps <- .Machine$double.eps^(2/3)
    data <- eval(object$call$data, environment(object$terms)) 
    n <- nrow(data) #grab to determine optimal bandwidth for sparsity estimation 

    k <- length(getcoef(object))
    coef <- simpar[,1:k]
    if(k < ncol(x))
        x <- as.data.frame(x[,names(coef(object)),drop=FALSE])

    ev <- coef %*% t(x)
    qi <- list(ev=ev)
    qi$pr <- qi$ev
    qi.name <- list(ev=paste("Expected Quantile Values: Q(tau=",tau,"|X)"))

    #Estimate sparsity to get variance estimate for fundamental uncertainty
    #Approximates using a difference estimate
    h <- bandwidth.rq(tau, n) #estimate optimal bandwidth for sparsity
    if(tau+h > 1) stop("tau+h > 1. Sparsity estimate failed. Please specify a tau closer to 0.5")
    if(tau-h < 0) stop("tau-h < 1. Sparsity estimate failed. Please specify a tau cloer  to 0.5")
    beta_high <- rq(object$formula, data=data, tau=tau+h)$coef
    beta_low <- rq(object$formula, data=data, tau=tau-h)$coef
    F_diff <- x %*% (beta_high-beta_low)
    if(any(F_diff <= 0))
        warning(paste(sum(F_diff <= 0), "density estimates were non-positive. Predicted values will likely be non-sensical."))
    f <- pmax(0, (2*h)/(F_diff-eps)) #Includes machine error correction as per summary.rq for nid case

    #Use asymptotic approximation of Q(tau|X,beta) distribution
    for(i in 1:nrow(ev))
        #Asymptotic distribution as per Koenker 2005 _Quantile Regression_ p. 72
        qi$pr[i,] <- rnorm(length(ev[i,]), mean=ev[i,], sqrt((tau*(1-tau)))/(f*sqrt(n)))
    qi.name$pr <- paste("Predicted Quantile Values: Q(tau=",tau,"|X)")
    if(!is.null(x1)){
        if(k < ncol(x1))
            x1 <- as.data.frame(x1[,names(coef(object)),drop=FALSE])
        ev1 <- coef %*% t(x1)
        qi$fd <- ev1-ev
        qi.name$fd <- paste("First Differences in Expected Quantile Values: Q(tau=",tau,"|X1)-Q(tau=",tau,"|X)")
    }

    if(!is.null(y)){
        stop("Conditional inference is not supported in rq.")
        #yvar <- matrix(rep(y, nrow(simpar)), nrow=nrow(simpar), byrow=TRUE)
        #qi$att.ev <- matrix(apply(yvar-qi$ev, 1, mean), nrow=(simpar))
        #qi$att.pr <- matrix(apply(yvar-qi$pr, 1, mean), nrow=(simpar))
        #qi.name$att.ev <- paste("Average Treatment Effect for the Treated: Y - Expected Q(tau=",tau,"|X)")
        #qi.name$att.pr <- paste("Average Treatment Effect for the Treated: Y - Predicted Q(tau=",tau,"|X)")
    }
    list(qi=qi, qi.name=qi.name)
}
