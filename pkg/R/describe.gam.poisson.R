describe.poisson.gam<-function(){
category <- "count"
description  <- "Generalized Additive Model for Event Count Dependent Variables"
authors <- c("Skyler J. Cranmer")
year <- 2007
package <-list(	name 	="mgcv",
		version	="0.1"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
