param.eiRxC <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) 
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
  
  else 
    coef <- coef(object)
  coef
}




