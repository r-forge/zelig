describe.quantile <- function(){
	category <- "continuous"
	description <- "Quantile Regression for Continuous Dependent Variables"
	authors <- c("Alexander D'Amour")
	year <- 2008
	package <- list( name		="quantreg",
					 version	="4.26"
				   )
	parameters <- list(xi="xi")
	parameters$xi <- list(equations=c(1,1),
					tagsAllowed=FALSE,
					depVar=TRUE,
					expVar=TRUE
					)

	list(category = category, authors = authors, year = year, description = description, package = package, parameters=parameters)
}
