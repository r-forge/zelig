sim.cond <- function(object, x, x1=NULL, num=c(1000, 100),
                     qoi = c("ev", "pr"), prev = NULL,
                     bootstrap = FALSE, bootfn=NULL, ...) {
  if (!is.null(x1)) {
    warning("First Differences are not calculated in conditional prediction models.")
    x1 <- NULL
  }
  xvar <- model.matrix(object, data = x)
  yvar <- model.response(x)
  class(xvar) <- c("matrix", "cond")
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  if (is.null(prev)) {
    if (!bootstrap & any(class(object) != "relogit")) 
      simpar <- param(object, num=num, bootstrap=bootstrap)
    else if (any(class(object) == "relogit")) 
      simpar <- param.relogit(object, num=num, x=xvar,
                              bootstrap=bootstrap, bootfn=bootfn, ...) 
    else {
      tt <- terms(object)
      dta <- eval(object$data, sys.parent())
      dta <- dta[complete.cases(model.frame(tt, dta)),]
      if (is.null(bootfn)) 
        bootfn <- bootfn.default
      res <- boot(dta, bootfn, R = num, object = object, ...)
      colnames(res$t) <- names(res$t0)
      simpar <- res$t
    }
  } else {
    if (bootstrap)
      stop("Error: Choosing 'bootstrap = TRUE' generates new parameters.  \nIf you wish to use previously generated parameters, \nplease specify only 'prev'.")
    else
      simpar <- prev
  }
  fn <- paste("zelig4", getzelig(object), sep = "")
  if(exists(fn)){
    if(!bootstrap)
      simpar <- do.call(fn, list(object=object, simpar=simpar, x=xvar, x1=x1, bootstrap=bootstrap, bootfn=bootfn))
    else
	simpar <- do.call(fn, list(object=object, simpar=simpar, x=xvar, x1=x1, bootstrap=bootstrap, bootfn=bootfn, dta=dta))
  } 
  simqi <- qi(object, simpar = simpar, x = xvar, x1 = x1, y = yvar)
  class(xvar) <- c("matrix", "cond")
  ca <- match.call()
  ca$num <- num
  res <- list(x=xvar, x1=x1, call = ca, zelig.call = getcall(object),
              par = simpar, qi=simqi$qi, qi.name=simqi$qi.name)
  class(res) <- "zelig"
  res
}







