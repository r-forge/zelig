qi.eiRxC <- function(object, simpar, x=NULL, x1=NULL, y=NULL, 
                     user.fn=NULL) {
        if (!is.null(x1))
          warning ("no first difference are available for EI models")
        if (!is.null(x))
          object$covar <- x
        ev <- calc.fractions(object, simpar)

        ev <- aperm(ev, perm = c(3,1,2))
        class(ev) <- c("ei", "array")
        qi <- list(ev=ev)
        qi.name <- list(ev = "Expected Values: E(Y|X)")
        list(qi=qi, qi.name=qi.name)

}
