zelig4gamma.gee <- function(object, simpar, x, x1=NULL, bootstrap = FALSE, bootfn = NULL, dta = NULL){
	num <- nrow(simpar)
	coef <- simpar
	eta <- coef %*% t(x)
	if(!is.null(x1))
		eta1 <- coef %*% t(x1)
	else
		eta1 <- NULL

	good.params <- function(par, x, x1=NULL){
		eta <- par %*% t(x)
		if(!is.null(x1)){
			eta1 <- par %*% t(x1)
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
		sum.neg <- length(which(apply(eta<=0,1,any)))
		coef <- good.params(par=coef, x=x, x1=x1)
		counter <- 1
		while(sum.neg > 0){
			if(!bootstrap)
				new.coef <- matrix(mvrnorm(sum.neg, mu = coef(object), Sigma = vcov(object)), nrow=sum.neg)
			else
				new.coef <- matrix(boot(dta, bootfn, R = sum.neg, object = object)$t, nrow=sum.neg)
				
			new.coef <- good.params(par=new.coef, x=x, x1=x1)
			coef <- rbind(coef, new.coef)	
			sum.neg <- num - nrow(coef)
			counter <- counter + 1
			if(counter==200)
				warning(paste("Suitable parameters not found after 200 iterations of rejection sampling.  Iterations will continue, but choosing another x is suggested for non-conditional prediction models."))
			if(counter==2000)
				stop("Rejection sampling stopped after 2000 iterations.  Please choose another x value.")
		}
	}
	res <- coef
	return(res)
}