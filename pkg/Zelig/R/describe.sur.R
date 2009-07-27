describe.sur<-function(){
  category <- "continuous"
description  <- "Seemingly Unrelated Regression"
authors <- c("Ferdinand Alimadhi","Ying Lu", "Elena Villalon")
year <- 2007
package <-list(	name 	="systemfit",
		version	="0.8"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(2,Inf),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
