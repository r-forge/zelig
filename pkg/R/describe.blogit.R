describe.blogit<-function(){
category <- "dichotomous"
description  <- "Bivariate Logistic Regression for Dichotomous Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="VGAM",
		version	="0.6"
		)
parameters<-list(mu="mu",phi="phi")
parameters$mu<-list(equations=c(2,2),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$phi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
