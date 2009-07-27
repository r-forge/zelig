describe.relogit<-function(){
category <- "dichotomous"
description  <- "Rare Events Logistic Regression for Dichotomous Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="stats",
		version	="0.1"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
