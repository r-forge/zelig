param.MCMCZelig <- function(object, num = NULL, bootstrap = FALSE) {
  if (bootstrap) 
    stop("For the class of MCMC models, no need to use Bootstrap method.")
   else 
    res <- object$coefficients


  res
  
}




