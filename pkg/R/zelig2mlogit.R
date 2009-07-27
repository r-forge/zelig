zelig2mlogit <- function(formula, model, data, M, ...) {
        mf <- match.call(expand.dots = TRUE)
        mf[[1]] <- VGAM::vglm 
        mf$family <- VGAM::multinomial
        formula<-parse.formula(formula,model,data)
        tt<-terms(formula)
        fact<-attr(tt,"depFactors")$depFactorVar
        ndim<-length(attr(tt,"depFactors")$depLevels)
        tmp <- cmvglm(formula, mf$model, ndim,data,fact)
        mf$formula <- tmp$formula  
        mf$constraints <- tmp$constraints
        mf$model <- mf$M <- NULL
        as.call(mf)
}
