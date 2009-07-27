zelig2normal.survey <- function(formula, model, data, M, 
                                weights=NULL, 
                                ids=NULL, probs=NULL, strata = NULL,  
                                fpc=NULL, nest = FALSE, check.strata = !nest, 			
                                repweights = NULL, 				
                                type, combined.weights=FALSE, rho = NULL, bootstrap.average=NULL, 
                                scale=NULL, rscales=NULL, fpctype="fraction",
                                return.replicates=FALSE,    			
                                na.action="na.omit", start=NULL, etastart=NULL, 
                                mustart=NULL, offset=NULL, 	      		
                                model1=TRUE, method="glm.fit", x=FALSE, y=TRUE, contrasts=NULL,
                                design=NULL){
        
        mf <- match.call(expand.dots = TRUE)					
        mf$M <- mf$model <- NULL    							
        
        if(is.null(ids)){ids<-~1}

        if(is.null(repweights)){
						
                mf$design <- svydesign(data=data, ids=ids, probs=probs,		
                                       strata=strata, fpc=fpc, nest=nest, check.strata=check.strata,
						   weight=weights)  ### added conventional weights input here
                
                mf$weights <- mf$ids <- mf$probs <- mf$strata <- mf$fpc <- NULL
                mf$nest <- mf$check.strata <- mf$repweights <- mf$type <- NULL
                mf$combined.weights <- mf$rho <- mf$bootstrap.average <- NULL
                mf$scale <- mf$rscales <- mf$fpctype <- mf$return.replicates <- NULL
                mf$data <- NULL
                
        } else {		
        assign(".survey.prob.weights", weights, envir = globalenv())	 ### moved global assignment down here
                mf$design <- svrepdesign(data=data, repweights=repweights, 	
                                         type=type, weights=weights, combined.weights=combined.weights, 
                                         rho=rho, bootstrap.average=bootstrap.average, scale=scale, 
                                         rscales=rscales, fpctype=fpctype, fpc=fpc)
                                        # ...drop extraneous options
                mf$weights <- mf$ids <- mf$probs <- mf$strata <- mf$fpc <- NULL
                mf$nest <- mf$check.strata <- mf$repweights <- mf$type <- NULL
                mf$combined.weights <- mf$bootstrap.average <- NULL
                mf$scale <- mf$rscales <- mf$fpctype <- NULL
                mf$data <- NULL
        }							
        
        mf[[1]] <- as.name("svyglm")
        as.call(mf)    
				
}