describe.probit<-function(){
category <- "dichotomous"
description  <- "Probit Regression for Dichotomous Dependent Variables"
authors <- c()
year <- 2007

package <-list(	name 	="stats",
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
