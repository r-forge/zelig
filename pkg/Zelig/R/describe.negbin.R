describe.negbin<-function(){
category <- "count"
description  <- "Negative Binomial Regression for Event Count Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="MASS",
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
