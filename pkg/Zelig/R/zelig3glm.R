zelig3logit <- zelig3probit <- zelig3normal <- zelig3gamma <-
  zelig3poisson <- zelig3negbin <- zelig3relogit <-
  function(res, fcall = NULL, zcall = NULL) {
    rob <- eval(zcall$robust)
    if (!is.null(rob)) {
      require(sandwich)
      if (is.list(rob)) {
        if (!any(rob$method %in% c("vcovHAC", "kernHAC", "weave")))
          stop("such a robust option is not supported")
        else {
          class(res) <- c("glm.robust", class(res))    
          res$robust <- rob
        }
      }
      else if (!is.logical(rob)) 
        stop("invalid input for robust.  Choose either TRUE or a list of options.")
      else if (rob) 
        class(res) <- c("glm.robust", class(res))    
    }
    return(res)
  }

