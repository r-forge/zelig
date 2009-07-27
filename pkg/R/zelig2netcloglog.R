zelig2cloglog.net <- function(formula, model, data, M, LF="cloglog", ...) {
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("netbinom")	
	mf$M <- mf$model  <- NULL
	mf$formula <- formula
	mf$LF <- "cloglog"
	as.call(mf)
}
