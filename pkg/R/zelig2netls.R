zelig2ls.net <- function(formula, model, data, M, ...) {
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("callnetlm")	
	mf$M <- mf$model  <- NULL
	mf$formula <- formula
	as.call(mf)
}
