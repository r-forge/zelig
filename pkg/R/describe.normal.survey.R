describe.normal.survey<-function(){
category <- "continuous"
description  <- "Survey-Weighted Normal Regression for Continuous Dependent Variables"
authors <- c("Nicholas Carnes")
year <- 2008
package <-list(name 	="survey",
		version	="3.6-13"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
