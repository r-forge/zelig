describe.ls.net<-function(){
category <- "continuous"
description  <- "Social Network Least Squares Regression for Continuous Dependent Variables"
authors <- c("Skyler J. Cranmer")
year <- 2007
package <-list(	name 	="sna",
		version	="1.4"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
