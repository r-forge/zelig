zelig4gamma <- function(object, simpar, x, x1=NULL, bootstrap = FALSE, bootfn = NULL, dta = NULL){
	k <- length(getcoef(object))
	num <- nrow(simpar)
	coef <- simpar[,1:k]
    	alpha <- simpar[,k+1]
	eta <- coef %*% t(x)
	if(!is.null(x1))
		eta1 <- coef %*% t(x1)
	else
		eta1 <- NULL

	good.params <- function(par, x, x1=NULL){
		eta <- par[,1:k] %*% t(x)
		if(!is.null(x1)){
			eta1 <- par[,1:k] %*% t(x1)
			pos <- which(eta>0 & eta1>0)
		}
		else{
			pos <- which(apply(eta > 0,1,all))
		}
		params <- matrix(par[pos,], nrow=length(pos), ncol=ncol(par))
		return(params)
	}


	if(length(which(apply(eta<=0,1,any)))>0 | (!is.null(eta1) & any(eta1<=0))){
		warning(paste("Negative expected values in simulations.  Rejection sampling method used."))
		if(!is.null(eta1))
			sum.neg <- length(unique(c(which(apply(eta<=0,1,any)), which(apply(eta1<=0,1,any)))))
		else
			sum.neg <- length(which(apply(eta<=0,1,any)))
		if(!bootstrap)
			coef <- good.params(par=coef, x=x, x1=x1)
		else
			simpar <- good.params(par=simpar, x=x, x1=x1)
		counter <- 1
		while(sum.neg > 0){
			if(!bootstrap){
				new.coef <- matrix(mvrnorm(sum.neg, mu = coef(object), Sigma = vcov(object)), nrow=sum.neg)
				new.coef <- good.params(par=new.coef, x=x, x1=x1)
				coef <- rbind(coef, new.coef)	
				sum.neg <- num - nrow(coef)
			}
			else{
				new.simpar <- matrix(boot(dta, bootfn, R = sum.neg, object = object)$t, nrow=sum.neg)
				new.simpar <- good.params(par=new.simpar, x=x, x1=x1)
				simpar <- rbind(simpar, new.simpar)
				sum.neg <- num - nrow(simpar)
			}
			counter <- counter + 1
			if(counter==200)
				warning(paste("Suitable parameters not found after 200 iterations of rejection sampling.  Iterations will continue, but choosing another x is suggested for non-conditional prediction models."))
			if(counter==2000)
				stop("Rejection sampling stopped after 2000 iterations.  Please choose another x value.")
		}
	}
	if(!bootstrap & any(alpha<=0)){
		pos <- which(alpha > 0)
		alpha <- alpha[pos]
		sum.neg <- num-length(pos)
		while(sum.neg > 0){
			new.alpha <- rnorm(sum.neg, mean = gamma.shape(object)$alpha, sd = gamma.shape(object)$SE)
			pos <- which(new.alpha > 0)
			alpha <- c(alpha, new.alpha[pos])
			sum.neg <- num - length(pos)
		}
	}
	if(!bootstrap){
		res <- cbind(coef, alpha)
	}
	else{
		res <- simpar
	}
	return(res)
}