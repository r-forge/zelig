\name{parse.par}
\alias{parse.par}

\title{Select and reshape parameter vectors}

\description{ The \code{parse.par} function reshapes parameter vectors for
comfortability with the output matrix from \code{\link{model.matrix.multiple}}. 
Use \code{parse.par} to identify sets of parameters; for example, within
optimization functions that require vector input, or within \code{qi}
functions that take matrix input of all parameters as a lump.  
}

\usage{
parse.par(par, terms, shape = "matrix", eqn = NULL)
}

\arguments{
\item{par}{the vector (or matrix) of parameters}
\item{terms}{the terms from either \code{\link{model.frame.multiple}} or 
\code{\link{model.matrix.multiple}}}
\item{shape}{a character string (either \code{"matrix"} or \code{"vector"})
that identifies the type of output structure}
\item{eqn}{a character string (or strings) that identify the
parameters that you would like to subset from the larger \code{par}
structure}
}

\value{
A matrix or vector of the sub-setted (and reshaped) parameters for the specified
parameters given in \code{"eqn"}.   By default, \code{eqn = NULL}, such that all systematic
components are selected.  (Systematic components have \code{ExpVar = TRUE} in the appropriate 
\code{describe.model} function.)  

If an ancillary parameter (for which \code{ExpVar = FALSE} in
\code{describe.model}) is specified in \code{eqn}, it is
always returned as a vector (ignoring \code{shape}).  (Ancillary
parameters are all parameters that have intercept only formulas.)  
}
\examples{
# Let's say that the name of the model is "bivariate.probit", and
# the corresponding describe function is describe.bivariate.probit(), 
# which identifies mu1 and mu2 as systematic components, and an 
# ancillary parameter rho, which may be parameterized, but is estimated 
# as a scalar by default.  Let par be the parameter vector (including 
# parameters for rho), formulae a user-specified formula, and mydata
# the user specified data frame.  

# Acceptable combinations of parse.par() and model.matrix() are as follows:
## Setting up
\dontrun{
data(sanction)
formulae <- cbind(import, export) ~ coop + cost + target
fml <- parse.formula(formulae, model = "bivariate.probit")
D <- model.frame(fml, data = sanction)
terms <- attr(D, "terms")

## Intuitive option
Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))
X <- model.matrix(fml, data = D, shape = "stacked", eqn = c("mu1", "mu2")
eta <- X %*% Beta

## Memory-efficient (compact) option (default)
Beta <- parse.par(par, terms, eqn = c("mu1", "mu2"))    
X <- model.matrix(fml, data = D, eqn = c("mu1", "mu2"))
eta <- X %*% Beta

## Computationally-efficient (array) option
Beta <- parse.par(par, terms, shape = "vector", eqn = c("mu1", "mu2"))
X <- model.matrix(fml, data = D, shape = "array", eqn = c("mu1", "mu2"))
eta <- apply(X, 3, '%*%', Beta)
}}

\seealso{\code{\link{model.matrix.multiple}}, \code{\link{parse.formula}} and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}}
 
\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}

\keyword{utilities}
