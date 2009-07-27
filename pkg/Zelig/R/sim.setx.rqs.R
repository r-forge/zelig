sim.setx.rqs <- function(object, x, x1=NULL, num= c(1000, 100), bootstrap=FALSE, ...){
    if (length(num)==2) {
        if(!bootstrap)
            num <- num[1]
        else
            num <- num[2]
    }
    res <- list()
    object <- stratify.rqs(object)
    N <- length(object)
    lev <- names(object)
    for(i in 1:length(lev)) {
        numN <- round(num/N)   
        res[[i]] <- sim(object[[i]], x=x[[i]], x1=x1[[i]], num=numN,
                        bootstrap=bootstrap, ...)
    }
    class(res) <- "zelig.rqs.strata"
    names(res) <- lev 
    res
}
