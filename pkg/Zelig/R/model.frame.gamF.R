model.frame.gamF <- function(formula, data, ...){
		gp <- interpret.gam(formula)
		ff<- gp$fake.formula
		return(model.frame.default(ff, data))
	}