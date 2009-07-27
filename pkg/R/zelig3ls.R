zelig3ls <- function(res, fcall = NULL, zcall = NULL) {
  rob <- eval(zcall$robust)
  if (!is.null(rob)) {
    require(sandwich)
    if (is.list(rob)) {
      if (!any(rob$method %in% c("vcovHC", "vcovHAC",
                                          "kernHAC", "weave")))
        stop("such a robust option is not supported")
      res$robust <- rob
      class(res) <- c("lm.robust", class(res))    
    }
    else if (!is.logical(rob)) 
      stop("invalid input for robust.  Choose either TRUE or a list of options.")
    else if (rob) 
      class(res) <- c("lm.robust", class(res))    
  }
  rc <- class(res)
  if("mlm" %in% class(res))
    class(res) <- c("zmlm", rc)
  return(res)
}

