describe.coxph<-function(){
  category <- "bounded"
  description  <- "Cox Proportional Hazard Regression for Duration Dependent Variables"
  authors <- c("Patrick Lam")
  year <- 2007
  package <- list(name ="survival",
		version	="2.34"
		)
  parameters <- list()
  parameters$mu<-list(equations=c(1,1),
                      tagsAllowed=FALSE,
                      depVar=TRUE,
                      expVar=TRUE,
                      specialFunction="Surv",
                      varInSpecialFunction=c(2,2)
		)
			
  list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
