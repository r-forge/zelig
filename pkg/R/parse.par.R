parse.par <- function(par, terms,shape = "matrix", eqn=NULL) {
  "%w/o%" <- function(x,y) x[!x %in% y]
  if (is.null(shape)) {
    if (any(class(terms) == "multiple"))
      shape <- "matrix"
    else
      shape <- "vector"
  }
  if(is.null(eqn))
    eqn<-attr(terms,"systEqns")
  if (!shape %in% c("matrix", "vector"))
    stop("not a valid 'shape' for parameters.  Choose from \"matrix\" or \"vector\".")
  if (any(class(terms) == "multiple")) {
    allidx <- make.parameters(terms = terms, shape = "vector")
    idx <- make.parameters(terms = terms,eqn=eqn, shape = "vector")
    mat <- t(make.parameters(terms = terms,eqn=eqn, shape = "matrix"))
    if(length(par)==length(allidx))
      par.names<-allidx
    else
      par.names<-idx
    ancil <-attr(terms,"ancilEqns")
    syst<-attr(terms,"systEqns")
    if (length(syst) == 1)
      shape <- "vector"
    if (any(eqn %in% ancil)) {
      if (any(eqn %in% syst)) {
        stop("  eqn cannot include both systematic and ancillary \n  parameters at the same time.")
      }
      else
        ret.val <- par[par.names %in% idx]
    }
    else { ## if eqn to be returned is a systematic component
      subs<-mat
      out <- matrix(0, nrow = nrow(subs), ncol = ncol(subs),
                    dimnames = dimnames(subs))
      for(i in 1:nrow(out))
        for(j in 1:ncol(out))
          if(!is.na(subs[i,j]))
            out[i,j]<-par[par.names %in% subs[i,j]]
      if (shape == "matrix") 
        ret.val <- t(out)
      else {
        ret.val <- par[par.names %in% idx]
      }
    }
  }
  ret.val
}


