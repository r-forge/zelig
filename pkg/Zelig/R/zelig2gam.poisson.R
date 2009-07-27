zelig2poisson.gam <- function(formula, model, data, M, ...) {
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("gam")	
	mf$M <- mf$model  <- NULL
	class(formula) <- "gamF"
	mf$formula <- formula
	mf$family <- poisson()
	as.call(mf)
}
