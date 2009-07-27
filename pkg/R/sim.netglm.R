sim.logit.net <- function(object, x=NULL, x1=NULL, num=c(1000, 100),
                        prev = NULL, bootstrap = FALSE, bootfn=NULL,
                        cond.data = NULL, ...) {
  if (!is.null(x))
    x <- as.matrix(x)
  if (!is.null(x1))
    x1 <- as.matrix(x1)
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  else if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  if (is.null(prev)) {
    if (any(class(object) == "relogit")) 
      simpar <- param.netglm(object, num=num, x=x, bootstrap=bootstrap) 
    else if (!bootstrap)
      simpar <- param.netglm(object, num=num, bootstrap=bootstrap)
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
  }
  else {
    if (bootstrap)
      stop("Error: Choosing 'bootstrap = TRUE' generates new parameters.  \nIf you wish to use previously generated parameters, \nplease specify only 'prev'.")
    else
      simpar <- prev
  }
  simqi <- qi.netglm(object, simpar = simpar, x = x, x1 = x1, y = NULL)
  c <- match.call()
  c[[1]] <- as.name("sim")
  c$num <- num
  res <- list(x=x, x1=x1, call = c, zelig.call = object$call,
              par = simpar, qi=simqi$qi, qi.name=simqi$qi.name)
  class(res) <- "zelig"
  res
}