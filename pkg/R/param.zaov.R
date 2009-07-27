param.mlm <-function(object, num, bootstrap = FALSE) {
        mu <- coef(object)
        depVars <- colnames(mu)
        sigma <- vcov(object)
        mu <- matrix(mu, ncol=1)
        
        
        if (!bootstrap) {
                coef <- mvrnorm(num, mu=mu, Sigma=sigma)
                res <- append.sigma(object, coef,num,bootstrap)
                
        } else {
                coef <- coef(object)
                res <- append.sigma(object, coef,num,bootstrap)
                
        }
  
        res
}
 
append.sigma <- function(object, coef,num, bootstrap){
        smobj <- summary.zmlm(object)
        df <- object$df.residual
        mat <- as.matrix(coef)
        for(n in 1:length(smobj)){
                resp <- smobj[[n]]
                nm <- names(smobj)[n]

                if(!bootstrap){
                        sig2 <- resp$sigma^2
                        alpha <- sqrt(df*sig2/rchisq(num, df=df))
                        mat <- cbind(mat, alpha)
                        nc <- ncol(mat)
                        del <- sub("([[:alpha:]+])[[:space:]+](.*)","\\1", nm)
                        cnm <- sub(paste(del, "[[:space:]+](.*)", sep=""),"\\1", nm)
                        cnm <- paste(cnm,":alpha",sep="")
                        colnames(mat)[nc] <- cnm
                        
                }else{
                        alpha <- resp$sigma
                        mat <- cbind(mat, alpha)
                }
                
        }
        return(mat)
}
