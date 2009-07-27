zelig3relogit <- function(res, fcall = NULL, zcall = NULL) {

  if ("relogit2" %in% class(res)) {
    obj <- list()
    obj$lower.estimate <- zelig3relogit(res$lower.estimate, fcall =
                                        fcall, zcall = zcall)
    obj$upper.estimate <- zelig3relogit(res$upper.estimate, fcall =
                                        fcall, zcall = zcall)
    obj$upper.estimate$call <- obj$lower.estimate$call <- as.call(zcall)
    class(obj) <- class(res)
    return(obj)
  }
  
  zcall$robust <- eval(zcall$robust)

  if (is.null(zcall$robust)) {
    if (res$weighting) {
      warning("robust is set to TRUE because weighting is used")
      rob <- TRUE
    }
    else
      rob <- FALSE
  }
  else if (is.logical(zcall$robust)) {
    if (!zcall$robust & res$weighting) {
      rob <- TRUE
      warning("robust is set to TRUE because weighting is used")
    }
    else
      rob <- zcall$robust
  }
  else
    rob <- zcall$robust
  if (is.list(rob)) {
    require(sandwich)
    if (!any(rob$method %in% c("vcovHAC", "kernHAC", "weave")))
      stop("such a robust option is not supported")
    else {
      class(res) <- c("relogit", "glm.robust")    
      res$robust <- rob
    }
  }
  else if (!is.logical(rob)) 
    stop("invalid input for robust.  Choose either TRUE or a list of options.")
  else if (rob) {
    require(sandwich)
    class(res) <- c("relogit", "glm.robust")
  }
  return(res)
}

