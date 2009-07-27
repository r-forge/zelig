zelig3aov <- function(res, fcall = NULL, zcall = NULL) {
  
 if(length(attr(attributes(res)$terms, "specials")$Error))
   class(res) <- c("zaovlist", class(res))    
 if("maov" %in% class(res))
    class(res) <- c("zmaov", "zmlm", class(res))  
  return(res)
}

