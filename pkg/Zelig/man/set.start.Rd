\name{set.start}
\alias{set.start}
\title{Set starting values for all parameters}

\description{After using \code{\link{parse.par}} and \code{\link{model.matrix.multiple}}, use 
\code{set.start} to set starting values for all parameters.  By default, starting values are set to 0.  If 
you wish to select alternative starting values for certain parameters, use \code{\link{put.start}} after 
\code{set.start}.}

\usage{
set.start(start.val = NULL, terms)
}

\arguments{ 
\item{start.val}{user-specified starting values.  If \code{NULL} (default), the default 
starting values for all parameters are set to 0.} 
\item{terms}{the terms output from \code{\link{model.frame.multiple}}}
}

\value{
A named vector of starting values for all parameters specified in \code{terms}, defaulting to 0.  
}

\examples{
\dontrun{
fml <- parse.formula(formula, model = "bivariate.probit")
D <- model.frame(fml, data = data)
terms <- attr(D, "terms")
start.val <- set.start(start.val = NULL, terms)
}}

\seealso{\code{\link{put.start}}, \code{\link{parse.par}}, \code{\link{model.frame.multiple}}, and the 
full Zelig manual at \url{http://gking.harvard.edu/zelig}.}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}
\keyword{utilities}
