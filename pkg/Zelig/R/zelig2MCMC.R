zelig2ei.hier <- function(formula, model, data, M, ...) {
  packageConflicts("VGAM")

  mf <- match.call(expand.dots = TRUE)
  
  if (is.null(mf$verbose) || !mf$verbose){
          mf$verbose <- 0
  } else {
          if (is.null(mf$mcmc)) {
                  mcmc <- 50000
          } else {
                  mcmc <- mf$mcmc
          }
          if (is.null(mf$burnin)){
                  burnin <- 5000
          } else {
                  burnin <- mf$burnin
          }
          mf$verbose <- round((mcmc+burnin)/10)
  }
  
  mf$model <- mf$M <- NULL
  temp <- mcmcei(formula=formula, data=data)
  
  if ((any(temp<0)) || ((any(temp<1) && !any(temp==0) ) && any(temp>1)))
    stop("data need to be either counts or proportions.\n") 

  if (is.null(mf$N)) {
          if (all(temp>=0)){  #N is not needed
                  mf$r0 <- temp$r0
                  mf$r1 <- temp$r1
                  mf$c0 <- temp$c0
                  mf$c1 <- temp$c1
          } else {
                  stop("Needs total counts for inputs as porportion.\n")
          }
  } else if (((length(mf$N)!= nrow(data)) && (length(mf$N)!=1)) || (any(mf$N<1)))
    stop("N needs to have same length as the observations and be postive numbers\n.")
  else if ((all(temp<1)) && (all(mf$N>1))){
          mf$r0 <- round(temp$r0*mf$N)
          mf$r1 <- mf$N-mf$r0
          mf$c0 <- round(temp$c0*mf$N)
          mf$c1 <- mf$N-mf$c0
          
  }
  
  mf[[1]] <- MCMCpack::MCMChierEI
  as.call(mf)
}

zelig2ei.dynamic <- function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        mf <- match.call(expand.dots = TRUE)
  
        if (is.null(mf$verbose) || !mf$verbose) {
                mf$verbose <- 0
        } else {
                if (is.null(mf$mcmc))
                  mcmc <- 50000
                else
                  mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 5000
                else
                  burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
  
        mf$model <- mf$M <- NULL
        temp <- mcmcei(formula=formula, data=data)

        if ((any(temp<0)) || ((any(temp<1) && !any(temp==0) ) && any(temp>1)))
          stop("data need to be either counts or proportions.\n") 
        if (is.null(mf$N)) {
                if (all(temp>=0)){  #N is not needed
                        mf$r0 <- temp$r0
                        mf$r1 <- temp$r1
                        mf$c0 <- temp$c0
                        mf$c1 <- temp$c1
                }
                else stop("Needs total counts for inputs as porportion.\n")
        }
        else if (((length(mf$N)!= nrow(data)) && (length(mf$N)!=1)) || (any(mf$N<1)))
          stop("N needs to have same length as the observations and be postive numbers\n.")
        else if ((all(temp<1)) && (all(mf$N>1))){
                mf$r0 <- round(temp$r0*mf$N)
                mf$r1 <- mf$N-mf$r0
                mf$c0 <- round(temp$c0*mf$N)
                mf$c1 <- mf$N-mf$c0
         }
        mf[[1]] <- MCMCpack::MCMCdynamicEI
        as.call(mf)
}

zelig2logit.bayes <-  function(formula, model, data, M, ...) {
   packageConflicts("VGAM")

  mf <- match.call(expand.dots = TRUE)

  if (is.null(mf$verbose) || !mf$verbose)
    mf$verbose <- 0
  else {
          if (is.null(mf$mcmc))
            mcmc <- 10000
          else
            mcmc <- mf$mcmc
          if (is.null(mf$burnin))
            burnin <- 1000
          else
            burnin <- mf$burnin
          mf$verbose <- round((mcmc+burnin)/10)
  }
   
   mf$model <- mf$M <- NULL
   
   mf[[1]] <- MCMCpack::MCMClogit
   as.call(mf)
}

zelig2probit.bayes <-  function(formula, model, data, M, ...) {
   packageConflicts("VGAM")

  mf <- match.call(expand.dots = TRUE)
   
   if (is.null(mf$verbose) || !mf$verbose)
     mf$verbose <- 0
   else {
           if (is.null(mf$mcmc))
             mcmc <- 10000
           else mcmc <- mf$mcmc
           if (is.null(mf$burnin))
             burnin <- 1000
           else burnin <- mf$burnin
           mf$verbose <- round((mcmc+burnin)/10)
   }

   mf$model <- mf$M <- NULL
   
   mf[[1]] <- MCMCpack::MCMCprobit
   as.call(mf)
}

zelig2normal.bayes <-  function(formula, model, data, M, ...) {
   packageConflicts("VGAM")

   mf <- match.call(expand.dots = TRUE)
   mf$model <- mf$M <- NULL
   if (is.null(mf$verbose) || !mf$verbose)
     mf$verbose <- 0
   else {
           if (is.null(mf$mcmc))
             mcmc <- 10000
           else
             mcmc <- mf$mcmc
           if (is.null(mf$burnin))
             burnin <- 1000
           else
             burnin <- mf$burnin
           mf$verbose <- round((mcmc+burnin)/10)
      }

   mf[[1]] <- MCMCpack::MCMCregress
   as.call(mf)
}

zelig2poisson.bayes <-  function(formula, model, data, M, ...) {
   packageConflicts("VGAM")

  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
   if (is.null(mf$verbose) || !mf$verbose)
     mf$verbose <- 0
   else {
           if (is.null(mf$mcmc))
             mcmc <- 10000
           else
             mcmc <- mf$mcmc
           if (is.null(mf$burnin))
             burnin <- 1000
           else
             burnin <- mf$burnin
           mf$verbose <- round((mcmc+burnin)/10)
   }
   mf[[1]] <- MCMCpack::MCMCpoisson
   as.call(mf)
}

zelig2tobit.bayes <-  function(formula, model, data, M, ...) {
        packageConflicts("VGAM")

        mf <- match.call(expand.dots = TRUE)
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))
                  mcmc <- 10000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 1000
                else burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        
        mf$model <- mf$M <- NULL
        mf[[1]] <- MCMCpack::MCMCtobit
        as.call(mf)
}

zelig2mlogit.bayes <-  function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        require(stats)
        mf <- match.call(expand.dots = TRUE)
        mf$model <- mf$M <- NULL
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))
                  mcmc <- 10000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 1000
                else
                  burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        
        mf[[1]] <- MCMCpack::MCMCmnl
        as.call(mf)
}

zelig2oprobit.bayes <-  function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        require(stats)
        mf <- match.call(expand.dots = TRUE)
        mf$model <- mf$M <- NULL
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))
                  mcmc <- 10000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 1000
                else
                  burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        
        mf[[1]] <- MCMCpack::MCMCoprobit
        as.call(mf)
}


zelig2factor.bayes <- function(formula, model, data, M, ...) {
   packageConflicts("VGAM")

  mf <- match.call(expand.dots = TRUE)
   if (is.null(mf$verbose) || !mf$verbose)
     mf$verbose <- 0
   else {
           if (is.null(mf$mcmc))
             mcmc <- 20000
           else
             mcmc <- mf$mcmc
           if (is.null(mf$burnin))
             burnin <- 1000
           else burnin <- mf$burnin
           mf$verbose <- round((mcmc+burnin)/10)
   }
   
   if (is.null(mf$factors))
     mf$factors<-2
   else if (mf$factors<2) stop("number of factors needs to be at
    least 2")
   mf$model <- mf$M <- NULL
   mf$x <- as.matrix(model.response(model.frame(formula, data=data, na.action=NULL)))
   mf[[1]] <- MCMCpack::MCMCfactanal
   as.call(mf)
}

zelig2factor.ord <- function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        mf <- match.call(expand.dots = TRUE)
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))
                  mcmc <- 20000
                else
                  mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 1000
                else
                  burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        if (is.null(mf$factors)) mf$factors<-2
        else if (mf$factors<1) stop("number of factors needs to be at
    least 1")
        mf$model <- mf$M <- NULL
        mf$x <- as.matrix(model.response(model.frame(formula, data=data, na.action=NULL)))
        
        mf[[1]] <- MCMCpack::MCMCordfactanal
        as.call(mf)
}

zelig2factor.mix <- function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        mf <- match.call(expand.dots = TRUE)
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))
                  mcmc <- 10000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin))
                  burnin <- 1000
                else
                  burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        
        if (is.null(mf$factors))
          mf$factors<-2
        else if (mf$factors<1)
          stop("number of factors needs to be at
    least 1")
        mf$model <- mf$M <- NULL
        
        var <- model.response(model.frame(formula, data=data,
                                          na.action=NULL))
        varnames <- colnames(var)
        mf$x <- as.formula(paste("~", paste(varnames, collapse="+")))
        mf[[1]] <- MCMCpack::MCMCmixfactanal
        
        as.call(mf)
}


zelig2irt1d <- function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        mf <- match.call(expand.dots = TRUE)
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))  mcmc <- 20000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin)) burnin <- 1000
                else burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        
        mf$model <- mf$M <- NULL
        mf$datamatrix <- as.matrix(model.response(model.frame(formula, data=data,
                                                              na.action=NULL)))
        mf$datamatrix <- t(mf$datamatrix)
        mf[[1]] <- MCMCpack::MCMCirt1d
        
        as.call(mf)
}


zelig2irtkd <- function(formula, model, data, M, ...) {
        packageConflicts("VGAM")
        
        mf <- match.call(expand.dots = TRUE)
        if (is.null(mf$verbose) || !mf$verbose)
          mf$verbose <- 0
        else {
                if (is.null(mf$mcmc))  mcmc <- 10000
                else mcmc <- mf$mcmc
                if (is.null(mf$burnin)) burnin <- 1000
                else burnin <- mf$burnin
                mf$verbose <- round((mcmc+burnin)/10)
        }
        if (is.null(mf$dimensions)) mf$dimensions <- 1
        
        mf$model <- mf$M <- NULL
        mf$datamatrix <- as.matrix(model.response(model.frame(formula, data=data,
                                                              na.action=NULL)))
        mf$datamatrix <- t(mf$datamatrix)
        mf[[1]] <- MCMCpack::MCMCirtKd
        
        as.call(mf)
}

