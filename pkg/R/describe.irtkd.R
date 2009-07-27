describe.irtkd<-function(){
category <- "dichotomous"
description  <- "K-Dimensional Item Response Model"
authors <- c("Ben Goodrich", "Ying Lu")
year <- 2007
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=FALSE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
