describe.gamma.survey<-function(){
category <- "bounded"
description  <- "Survey-Weighted Gamma Regression for Continuous, Positive Dependent Variables"
authors <- c("Nicholas Carnes")
year <- 2008
package <-list(	name 	="survey",
		version	="3.6-13"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
