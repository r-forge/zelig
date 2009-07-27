zelig3ei.hier <- zelig3ei.dynamic <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data

  if (!is.null(zcall$N))
    out$N <- zcall$N
  
  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  attr(out$terms,"intercept") <- 0
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed
  class(out) <- "MCMCZelig"

 out
}


zelig3logit.bayes <- zelig3oprobit.bayes <- zelig3poisson.bayes <-
  zelig3mlogit.bayes <- zelig3normal.bayes <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data

  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed

  class(out) <- "MCMCZelig"

 out
}

zelig3probit.bayes <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  if (is.null(zcall$bayes.resid)) 
    zcall$bayes.resid <- FALSE

  if (zcall$bayes.resid==FALSE)
    out$coefficients <- res
  else 
    {
      p<-dim(model.matrix(eval(zcall$formula), eval(zcall$data)))[2]
      out$coefficients <- res[,1:p]
      out$bayes.residuals <- res[, -(1:p)]
    }  
  
  out$formula <- zcall$formula
  out$data <- zcall$data

  out$model <- model.frame(formula=eval(out$formula),data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed

  class(out) <- "MCMCZelig"

 out

  }

  zelig3tobit.bayes <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  if (!is.null(zcall$below)) out$below <- zcall$below
  else out$below <- 0

  if (!is.null(zcall$above)) out$above <- zcall$above
  else out$above <- Inf 
 
  out$data <- zcall$data

  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed

  class(out) <- "MCMCZelig"

 out
}

  zelig3factor.bayes <- zelig3factor.ord <- zelig3factor.mix <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data
  
  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  attr(out$terms,"intercept") <- 0
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed

  class(out) <- "MCMCZelig"

 out
}


  zelig3irt1d <- zelig3irtkd <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data
  
  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  attr(out$terms,"intercept") <- 0
  if (is.null(zcall$seed)) out$seed <- NA
    else out$seed <- zcall$seed

  class(out) <- "MCMCZelig"

 out
}




