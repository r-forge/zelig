zelig2aov <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- FALSE
  mf$M <- mf$robust <- NULL
  mf[[1]] <- aov
#  ix <- grep("Error", as.character(formula))
#  if(length(ix)) class(formula) <- c(class(formula),"aoverrorF")
  mf$formula <- formula
  as.call(mf)
}

