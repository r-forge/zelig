zelig2gamma.gee <- function(formula, model, data, M, ...) {
  require(gee)
  mf <- match.call(expand.dots = TRUE)
  if(is.null(mf$corstr))
    mf$corstr <- as.character("independence")
  if(mf$corstr=="fixed" & is.null (mf$R))
    stop("R must be defined.")
  mf$model <- mf$M <- NULL
  mf[[1]] <- gee::gee
  if (is.character(mf$id))
    mf$id <- as.name(mf$id)
  mf$family <- Gamma
  mf$robust <- NULL
  as.call(mf)
}