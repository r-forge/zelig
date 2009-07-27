calllogit.net <- function(formula, data, family = binomial(link=logit), ...)
{
    Terms <- terms(formula)
    intercept.value <- attr(Terms, "intercept") 
    if (intercept.value > 0){
    	intercept = TRUE
    	}
    if (intercept.value == 0){
    	intercept = FALSE
    	} 
	if (missing(data)) 
			data <- environment(formula)		
	mf <- match.call(expand.dots = FALSE)	
    m <- match(c("formula", "data", "weights"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
	D <- model.frame(formula, data = data)
	y <- D[[1]]
	#x.array.names <- as.list(for(i in 2:length(D)) {names(D[[i]])})
	x <- array(dim=c((length(D) - 1), nrow(y), ncol(y)))
	for(i in 2:length(D)) {
		x[i - 1,,] <- D[[i]]	}
	if (intercept == TRUE){
	fit <- logit.net.zelig(y, x, intercept=TRUE,...)
		}
	if (intercept == FALSE){
	fit <- logit.net.zelig(y, x, intercept=FALSE, ...)
		}
	fit$names <- names(mf[2:stackcount(mf)])  # paste("x", 1:(nx - intercept), sep = "")
    if (intercept) 
        fit$names <- c("(intercept)", fit$names)
    fit$intercept <- intercept
	fit$xlevels <- .getXlevels(mt, mf)
	fit <- c(fit, list(call = call, formula = formula, terms = mt, 
	data = data, xlevels = .getXlevels(mt, mf)))
		new.data <- as.data.frame(as.vector(data[,1]))
	for(i in 2:ncol(data)){
	new.data <- cbind(new.data, as.vector(data[,i])) } 
	names(new.data) <- names(data)
	fit$zelig.data <- new.data
	fit$family <- family
	fit$rank <- fit$df.model
	so <- summary.glm(fit)
	fit$mod.coefficients <- so$coefficients
	fit$cov.unscaled <- so$cov.unscaled
	fit$cov.scaled <- so$cov.scaled
    class(fit) <- "logit.net"
    return(fit)
}
