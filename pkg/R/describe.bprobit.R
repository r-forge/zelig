describe.bprobit<-function(){
category <- "dichotomous"
description  <- "Bivariate Probit Regression for Dichotomous Dependent Variables"
authors <- c()
year <- 2007
package <-list(	name 	="VGAM",
		version	="0.6"
		)
parameters<-list(mu="mu", rho="rho")
parameters$mu<-list(equations=c(2,2),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$rho<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
