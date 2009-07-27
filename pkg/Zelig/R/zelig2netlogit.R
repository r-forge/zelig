zelig2logit.net <- function(formula, model, data, M, LF="logit", ...) {
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("netbinom")	
	mf$M <- mf$model  <- NULL
	mf$formula <- formula
	mf$LF <- "logit"
	as.call(mf)
}
