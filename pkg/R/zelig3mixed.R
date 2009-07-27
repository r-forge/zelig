## modified by delia/ferdi 09/22/08
############################

zelig3ls.mixed <- zelig3gamma.mixed <- zelig3poisson.mixed <- zelig3probit.mixed <- 
	zelig3logit.mixed <- function(res, fcall = NULL, zcall=NULL){
		res@nlmodel <- fcall@call
		return (res)
}