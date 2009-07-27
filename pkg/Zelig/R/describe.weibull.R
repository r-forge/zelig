describe.weibull<-function(){
category <- "bounded"
description  <- "Weibull Regression for Duration Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="survival",
		version	="2.2"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="Surv",
			varInSpecialFunction=c(2,2)
		)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
