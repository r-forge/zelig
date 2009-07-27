setx.zaovlist <- function(object,fn = list(numeric = mean, ordered =
                                   median, other = mode),
                          data = NULL,
                          cond = FALSE, counter = NULL, ...) {
        
        x <- setx.default(object, fn=fn,data=data,cond=cond,counter=counter,...)
        class(x) <- c("zaovlist", class(x))
        x
}
