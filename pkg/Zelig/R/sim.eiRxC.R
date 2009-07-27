###
##  sim function for eiRxC model.
##  NOTES:
##  - this model accepts only bootstrap = FALSE argument.
##  - the default bootfn will be "paramsei.estim"
##

sim.eiRxC <- function(object, x=NULL, x1=NULL, num= 100,
                        prev = NULL, bootstrap = TRUE, bootfn=NULL,
                        cond.data = NULL, ...) {
        if (!is.null(x))
          x <- as.matrix(x)

        if (!is.null(x1))
          warning ("no first difference are available for EI models")

        if (!is.null (prev))
          stop("Error: prev option is not supported for this model")

        if (!bootstrap)
          stop ("Error: please use bootstrapping to simulate parameters for this model")

        ## get the data to pass to boot function
        tt <- terms(object)
        dta <- eval(getcall(object)$data, sys.parent())
        dta <- dta[complete.cases(model.frame(tt, dta)),]
        if (is.null(bootfn))
          bootfn <- bootfn.default
        res <- boot(dta, bootfn, R = num, object = object, ...)
        #res <- boot(data= dta, statistic = bootfn, R = num, nR = object$dims[[1]], nC = object$dims[[2]], ...)
        colnames(res$t) <- names(res$t0)
        simpar <- res$t
        
    
        simqi <- qi(object, simpar = simpar, x = x, x1 = x1, y = NULL)
        c <- match.call()
        c[[1]] <- as.name("sim")
        c$num <- num
        res <- list(x=x, x1=x1, call = c, zelig.call = getcall(object),
                    par = simpar, qi=simqi$qi, qi.name=simqi$qi.name)
        class(res) <- "zelig"
        res
}
















