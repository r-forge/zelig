MIsimulation <- function (object, num = c(1000, 100), prev = NULL, 
                          bootstrap = FALSE, bootfn = NULL, x = NULL, x1 = NULL, ...)  {
  M <- length(object)
  simpar <- simqi <- res <- list()
  fn <- paste("zelig4", getzelig(object[[1]]), sep = "")
  if (is.null(prev)) {
    numM <- round(num/M)
    if (!bootstrap) {
      for (i in 1:M){
        simpar[[i]] <- param(object[[i]], num = numM, bootstrap =
                             bootstrap)
  	  if(exists(fn)){
	  	if(any(class(x)=="setx.cond")){
		  xvar <- as.matrix(cbind(1,x[[1]][,2:ncol(x[[1]])]))
		  for (h in 2:M)
      		xvar <- as.matrix(rbind(xvar, as.matrix(cbind(1,x[[h]][,2:ncol(x[[1]])]))))
              
		  simpar[[i]] <- do.call(fn, list(object=object[[i]], simpar=simpar[[i]], x=xvar, x1=NULL, bootstrap=bootstrap, bootfn=bootfn))       
		}
		else
    	  	  simpar[[i]] <- do.call(fn, list(object=object[[i]], simpar=simpar[[i]], x=x, x1=x1, bootstrap=bootstrap, bootfn=bootfn))      
	  }
	}
    }
    else {
      tt <- terms(object[[1]])
      if (is.null(bootfn)) 
        bootfn <- bootfn.default
      for (i in 1:M) {
        dta <- eval(object[[i]]$data, sys.parent())
        res <- boot(dta, bootfn, R = num, object = object[[i]],...)
        colnames(res$t) <- names(res$t0)
        simpar[[i]] <- res$t
	  if(exists(fn)){
	  	if(any(class(x)=="setx.cond")){
		  xvar <- as.matrix(cbind(1,x[[1]][,2:ncol(x[[1]])]))
		  for (h in 2:M)
      		xvar <- as.matrix(rbind(xvar, as.matrix(cbind(1,x[[h]][,2:ncol(x[[1]])]))))
              
		  simpar[[i]] <- do.call(fn, list(object=object[[i]], simpar=simpar[[i]], x=xvar, x1=NULL, bootstrap=bootstrap, bootfn=bootfn, dta=dta))       
		}
		else
    	  	  simpar[[i]] <- do.call(fn, list(object=object[[i]], simpar=simpar[[i]], x=x, x1=x1, bootstrap=bootstrap, bootfn=bootfn, dta=dta))      
	  }
	}
    }
    params <- as.matrix(simpar[[1]])
    for (j in 2:M)
      params <- rbind(params, as.matrix(simpar[[j]]))
  }
  else {
    if (bootstrap) 
      stop("Error: Choosing 'bootstrap = TRUE' generates new parameters.  \nIf you wish to use previously generated parameters, \nplease specify only 'prev'.")
    else params <- prev
  }
  params
}

