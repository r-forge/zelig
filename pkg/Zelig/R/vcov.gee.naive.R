vcov.gee.naive <- function(object, ...){
  class(object) <- c("gee", "glm")
  return(object$naive.variance)
}  

  