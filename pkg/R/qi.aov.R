qi.aov <- function(object, simpar, x, x1 = NULL, y = NULL) {
  ### Calls coef.aov that deletes all entries with  NA.
  ### qi.lm calls coef.default that do not delete entries with 
  ### NA yielding error messages
  
  cc <- coef(object)
  object$coef <- cc
  qi.lm(object, simpar, x, x1, y)
}
 
