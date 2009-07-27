\name{put.start}
\alias{put.start}
\title{Set specific starting values for certain parameters}

\description{ After calling \code{\link{set.start}} to create default starting values, use \code{put.start} 
to change starting values for specific parameters or parameter sets. }

\usage{
put.start(start.val, value, terms, eqn)
}

\arguments{
\item{start.val}{the vector of starting values created by \code{\link{set.start}}} 
\item{value}{the scalar or vector of replacement starting values}  
\item{terms}{the terms output from \code{\link{model.frame.multiple}}}
\item{eqn}{character vector of the parameters for which you would like to replace
the default values with \code{value}}
}

\value{A vector of starting values (of the same length as \code{start.val})}

\seealso{\code{\link{set.start}}, and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}.
}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>; Ferdinand Alimadhi
<\email{falimadhi@iq.harvard.edu}>
}


\keyword{utilities}

