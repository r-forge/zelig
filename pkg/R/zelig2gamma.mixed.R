zelig2gamma.mixed <- function(formula, model, data, M, ...){
        mf <- match.call(expand.dots=TRUE)
        mf[[1]]<- as.name("lmer")
        
        mf$formula <- tolmerFormat(reduceMI(formula))
        
        mf$model <- mf$M <- NULL
        
        if (is.null(mf$family)){
                mf$family <- Gamma(link="identity")
        }
        return(as.call(mf))
}

model.frame.lmer <- function(obj){
        obj@frame
}