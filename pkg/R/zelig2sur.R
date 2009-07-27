zelig2sur <- function(formula, model, data, M,...) {
        mf <- match.call(expand.dots = TRUE)
        mf[[1]] <- as.name("callsystemfit")
        formula<-parse.formula(formula,model)
        tt<-terms(formula)
        mf$method<-"SUR"
        mf$model<- mf$M<-NULL
        #mf$formula<-formula
        mf$formula<-formula[names(attr(tt,"depVars"))]
        as.call(mf)
}
