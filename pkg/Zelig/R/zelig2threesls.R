zelig2threesls <- function(formula, model, data, M,...) {
        "%w/o%" <- function(x,y) x[!x %in% y]
        mf <- match.call(expand.dots = TRUE)
        mf[[1]] <- as.name("callsystemfit")
        formula<-parse.formula(formula,model)
        tt<-terms(formula)
        ins<-names(tt) %w/o% names(attr(tt,"depVars"))
        if(length(ins)!=0)
          if(length(ins)==1)
            inst<-formula[[ins]]
          else inst<-formula[ins]
        else
          stop("threesls model requires instrument!!\n")
        mf$method<-"3SLS"
        mf$inst<-inst
        mf$model<- mf$M<-NULL
        mf$formula<-formula[names(attr(tt,"depVars"))]
        #class(mf$formula)<-c("multiple","list")
        as.call(mf)
}
