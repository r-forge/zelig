callnetlm <- function (formula, data, ...) 
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
	cl <- match.call()
	mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
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
	fit <- netlm(y, x, intercept=TRUE,...)
		}
	if (intercept == FALSE){
	fit <- netlm(y, x, intercept=FALSE, ...)
		}
	fit$names <- names(mf[2:stackcount(mf)])  
    if (intercept) 
        fit$names <- c("(intercept)", fit$names)
    fit$intercept <- intercept
	mm <- model.matrix(mt, mf, contrasts)
	fit$contrasts <- attr(x, "contrasts")
	fit$call <- cl
	fit$terms <- mt
	fit$model <- mf
	fit$mm <- mm
	fit$x <- x
	fit$y <- y
	fit$xlevels <- .getXlevels(mt, mf)
	fit <- c(fit, list(call = call, formula = formula, terms = mt, 
	data = data, xlevels = .getXlevels(mt, mf)))
	new.data <- as.data.frame(as.vector(data[,1]))
	for(i in 2:ncol(data)){
	new.data <- cbind(new.data, as.vector(data[,i])) } 
	names(new.data) <- names(data)
	fit$zelig.data <- new.data
	so <- summary.lm(fit)
	fit$sigma <- so$sigma
	fit$r.squared <- so$r.squared
	fit$adj.r.squared <- so$adj.r.squared
	fit$cov.unscaled <- so$cov.unscaled
	fit$mod.coefficients <- so$coefficients
	class(fit) <- "netlm"
    return(fit)
}
