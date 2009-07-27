zelig3logit.gee <- zelig3probit.gee <- zelig3normal.gee <- 
zelig3poisson.gee <- zelig3gamma.gee <- function(res, fcall=NULL, zcall=NULL){	
  rob <- eval(zcall$robust)	
  if (!is.null(rob)){		
    if (!is.logical(rob))	
      stop("invalid input for robust.  Choose either TRUE or FALSE.")
    else if(!rob)   		
      class(res) <- c("gee.naive", class(res))	
    else if (rob)		
      class(res) <- c("gee.robust", class(res))	
	}
  else {				
    class(res) <- c("gee.robust", class(res))	
	}
  return(res)
}