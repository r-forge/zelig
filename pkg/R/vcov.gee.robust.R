vcov.gee.robust <- function(object, ...){
  class(object) <- c("gee", "glm")
  return(object$robust.variance)
}  
