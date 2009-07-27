describe.probit.gam<-function(){
category <- "dichotomous"
description  <- "Generalized Additive Model for Dichotomous Dependent Variables"
authors <- c("Skyler J. Cranmer")
year <- 2007
package <-list(	name 	="mgcv",
		version	="0.1"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
