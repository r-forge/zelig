### DESCRIPTION: Simulation of quantities of interest for model=aovlist
###              object is of class = c(zaovlist,aovlist,listof)
###              It contains list elements for multiple strata, 
###              which individual class is c(aov,lm). 
###              A formula with an Error term describes the strata
###              for the "Accross" and "Within" aov-models.  
###              We extract those aov-models from object with relevant
###              information and apply sim.default to each of
###              them separately.
###
### OUTPUT a list after applying sim.default to each aov-model 
###        of multiple-strata object separately. 
###        The result is of class zeliglist because contains a list
###        of zelig simulation models.
###
### Elena Villalon (evillalon@iq.harvard.edu)

sim.zaovlist <- function(object, x=NULL, x1=NULL, num=c(1000, 100),
                         prev = NULL, bootstrap = FALSE,
                         bootfn=NULL, cond.data=NULL,...) {
                     
        tt <- attributes(object)$terms
        tt.attr <- attributes(tt)
        vars <- tt.attr$term.labels
        z.out <- object
        ## only those lists elements (or strata) that are relevant to sim()
        ind <- unlist(find.ind(object))
        if(length(ind))
          object <- object[ind]
   
        nm <- names(object)
   
        sout <- list()
        zcall <- getcall(z.out) 
 
        for(n in 1:length(object)){
                xn <- x
                x1n <- x1
                obj <- object[[n]]
                objcoef <- coefficients(obj)
                if(length(x))
                  xn <- pick.coef(objcoef,x)
                if(length(x1))
                  x1n <- pick.coef(objcoef,x1)
                
                
                if(bootstrap){
                        stop("Option bootstrap=TRUE with Error term is not yet supported") 
                }
   
                ss <- sim.default(obj,x=xn, x1=x1n, num=num,
                                  prev = prev, bootstrap = bootstrap,
                                  bootfn=bootfn,cond.data=cond.data, ...)
                
                ix <- grep("^zelig\\.call$", names(ss)$default)
                
                if(length(ix))
                  ss[[ix]] <- zcall
                else
                  ss <- c(ss,list(zelig.call=zcall))
                sout <- c(sout, list(ss))
        }
        names(sout) <- nm
        class(sout) <- c("zeliglist", class(sout))
        sout
}

###DESCRIPTION: object is a list of elements, each is class c("aov", "lm")
###             Find those elemnts of the list with relevant information
###
###INPUT: object, a list of class c("aovlist", "listof")
###OUTPUT: index to relevant list elements
###
### AUTHOR: Elena Villalon (evillalon@iq.harvard.edu)
###
find.ind <- function(object){
        nmobj <- names(object)
 
        ixa <- sapply(1:length(object), function(n){
                obj <- object[[n]]
                nm  <- nmobj[n]
                ixd <- c(grep("Intercept", nm),grep("^[Cc]all", nm))
                if(length(ixd))
                  return(NULL)
                resterms <- ifelse(!length(grep("terms", attributes(obj)$names)), NA, n)
                
                rescoef <- ifelse(!length(coefficients(obj)), NA, n)
                res <- NULL
                if(!is.na(resterms) && !is.na(rescoef))
                  res <- n
                return(res)
        })
        return(ixa)
}
   

###DESCRIPTION: objectcoef is vector with the names and values of
###             explanatory variables. Find the values of x applicable to
###             objcoef, i.e. that are also values of objcoef
###             There is no need to use this function in sim.zaovlist
###
###INPUT: objectcoef, the coefficients for obj of class c("aov", "lm")
###       x the result of setx, with values for explanatory variables
###
###OUTPUT: x but with only explanatory variables in objcoef 
###
### AUTHOR: Elena Villalon (evillalon@iq.harvard.edu)
###              
pick.coef <- function(objcoef,x){
      
        objnmcoef <- names(objcoef)
        objnmcoef <- sapply(objnmcoef,function(m) paste("^", m,"$", sep=""))
        if(length(x)) xnmcoef <- names(x)
        ix <- sapply(objnmcoef, grep, xnmcoef)
        ix <- unlist(ix)
        if (length(ix)) 
          x <- x[ix]
        return(x)
}
