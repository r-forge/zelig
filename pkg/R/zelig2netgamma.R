zelig2gamma.net <- function(formula, model, data, M, ...) {
        
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("gamma.net")	
	mf$M <- mf$model  <- NULL
	mf$formula <- formula
	as.call(mf)
}
