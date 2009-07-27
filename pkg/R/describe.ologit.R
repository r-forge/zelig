describe.ologit<-function(){
category <- "ordinal"
description  <- "Ordinal Logistic Regression for Ordered Categorical Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="MASS",
		version	="0.1"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="as.factor",
			varInSpecialFunction=c(1,1)
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
