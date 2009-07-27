getzelig <- function(x) {
  if (zeligS4check(x)) return(eval(x@call$model))
  else return(eval(x$call$model))
}

getcall <- function(x) {
  if (zeligS4check(x)) {
     return(x@call)
  } else {
      return(x$call)
  }
}

getcoef <- function(x) {
  if (zeligS4check(x)) {
    if ("coef3" %in% slotNames(x)) {
        return(x@coef3)
    } else {
        return(x@coef)
    }
  } else {
      return(x$coef)
  }
}

getdata <- function(x) {
  if (zeligS4check(x)) {
    if ("data" %in% slotNames(x)) {
      return(x@data)
    } else if ("model" %in% slotNames(x)){
       return(x@model)
    } else {
       return(NULL)
    }
  } else {
      return(x$zelig.data)
  }
}


zeligS4check <- function(obj){
   return(isS4(obj))
}
