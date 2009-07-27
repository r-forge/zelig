zelig3coxph <- function(res, fcall=NULL, zcall=NULL){	
  rob <- eval(zcall$robust)	
  if (!is.null(rob)){		
    if (!is.logical(rob))	
      stop("invalid input for robust.  Choose either TRUE or FALSE.")
    else if(!rob)   		
      class(res) <- c("coxph.naive", class(res))	
    else if (rob)		
      class(res) <- c("coxph.robust", class(res))	
	}
  else {				
    class(res) <- c("coxph.naive", class(res))	
	}
  return(res)
}
