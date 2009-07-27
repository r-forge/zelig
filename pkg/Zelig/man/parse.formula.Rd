\name{parse.formula}
\alias{parse.formula}

\title{Parsing user-input formulas into multiple syntax}

\description{Parse the input formula (or list of formulas) into the 
standard format described below.  Since labels for this format will vary 
by model, \code{parse.formula} will evaluate a function \code{describe.model},
where \code{model} is given as an input to \code{parse.formula}.

If the \code{describe.model} function has more than one parameter for
which \code{ExpVar = TRUE} and \code{DepVar = TRUE}, then the
user-specified equations must have labels to match those parameters,
else \code{parse.formula} should return an error. In addition, if the
formula entries are not unambiguous, then \code{parse.formula} returns an error.
}
  
\usage{
parse.formula(formula, model, data = NULL)
}

\arguments{
\item{formula}{either a single formula or a list of \code{formula} objects}  
\item{model}{a character string specifying the name of the model}
\item{data}{an optional data frame for models that require a factor response variable}
}

\value{The output is a list of formula objects with class 
\code{c("multiple", "list")}.  Let's say that the name of the model is 
\code{"bivariate.probit"}, and the corresponding describe function is 
\code{describe.bivariate.probit}, which identifies \code{mu1} and 
\code{mu2} as systematic components, and an ancillary parameter \code{rho}, which
may be parameterized, but is estimated as a scalar by default.  
}

\details{Acceptable user inputs are as follows:

\tabular{lll}{
                 \tab User Input   \tab Output from \code{parse.formula}\cr
\tab \tab \cr
Same covariates, \tab cbind(y1, y2) ~ x1 + x2 * x3  \tab list(mu1 = y1 ~ x1 + x2 * x3,\cr
separate effects \tab                               \tab      mu2 = y2 ~ x1 + x2 * x3,\cr
                 \tab                               \tab      rho = ~ 1)\cr
\tab \tab \cr
With \code{rho} as a \tab list(cbind(y1, y2) ~ x1 + x2, \tab list(mu1 = y1 ~ x1 + x2,\cr
systematic equation  \tab rho = ~ x4 + x5)              \tab      mu2 = y2 ~ x1 + x2,\cr
                     \tab                               \tab      rho = ~ x4 + x5)\cr
\tab \tab \cr
With constraints \tab list(mu1 = y1 ~ x1 + tag(x2, "x2"), \tab list(mu1 = y1 ~ x1 + tag(x2, "x2"),\cr
(same variable)  \tab      mu2 = y2 ~ x3 + tag(x2, "x2")) \tab      mu2 = y2 ~ x3 + tag(x2, "x2"),\cr
                 \tab                                     \tab      rho = ~ 1)\cr
\tab \tab \cr
With constraints \tab  list(mu1 = y1 ~ x1 + tag(x2, "z1"), \tab list(mu1 = y1 ~ x1 + tag(x2, "z1"),\cr
(different variables) \tab     mu2 = y2 ~ x3 + tag(x4, "z1")) \tab     mu2 = y2 ~ x3 + tag(x4, "z1"),\cr
                      \tab                                  \tab         rho = ~ 1)\cr
}}

\examples{
\dontrun{
data(sanction)
formulae <- list(cbind(import, export) ~ coop + cost + target)
fml <- parse.formula(formulae, model = "bivariate.probit")
D <- model.frame(fml, data = sanction)
}}

\seealso{
\code{\link{parse.par}}, \code{\link{model.frame.multiple}}, 
\code{\link{model.matrix.multiple}}, and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}.
}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi 
<\email{falimadhi@iq.harvard.edu}>
}

\keyword{utilities}










