zelig2normal.gam <- function(formula, model, data, M, ...) {
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("gam")	
	mf$M <- mf$model  <- NULL
	class(formula) <- "gamF"
	mf$formula <- formula
	as.call(mf)
}
