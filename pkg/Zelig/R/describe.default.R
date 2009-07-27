describe.default<-function(){
category <- "Dichotomous"
description  <- "A statistical model"

authors <- c("an author")
year <- "a year"

parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE)
			
list(category = category, authors = authors, year = year,description=description,parameters=parameters)
}
