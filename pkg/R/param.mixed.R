## modified by delia 09/22/08
################################

param.mer <- function(object, num, bootstrap=FALSE){
	fixed <- fixef(object)
	vars <- ranef(object, postVar=TRUE)
	gammas <- NULL
	n.G <- length(vars)
	
	if (!bootstrap){
		object <- selectMethod("summary", "mer")(object)
		# sample fixed effects
		betasF <- NULL
		vcov.fix <- vcov(object)
		if (length(fixed) > 0){
			betasF <- mvrnorm(num, fixed, vcov.fix)
		}
		# sample random effects
		for (m in 1:n.G){
			vars.m <- attr(vars[[m]], "postVar")
			V.beta <- VarCorr(object)[[m]]
			J <- dim(vars.m)[1]
			gammas[[m]] <- mvrnorm(num, rep(0, J), V.beta)
		}
	}
	else {
		object <- summary(object)
		# set fixed effects
		betasF <- fixed
		# sample random effects
		for (m in 1:n.G){
			V.beta <- VarCorr(object)[[m]]
			gammas[[m]]<- mvrnorm(1, 0, V.beta)
		}
	}
	
	names(gammas) <- names(vars)
	betas <- betasF
	scale <- object@sigma
	
	list(betas=betas, gammas=gammas, scale=scale)
}