print.zaovlist <- function (x, ...) 
{
    cl <- attr(x, "call")
    if (!is.null(cl)) {
      attr(x,"call") <- NULL
     
    }
       
    
    stats:::print.aovlist(x,...)
}
   
