\name{current.packages}

\alias{current.packages}

\title{Find all packages in a dependency chain}

\description{
  Use \code{current.packages} to find all the packages suggested or 
required by a given package, and the currently installed version number 
for each.  
}

\usage{
current.packages(package)
}

\arguments{
\item{package}{a character string corresponding to the name of an 
installed package}
}

\value{
  A matrix containing the current version number of the packages 
suggested or required by \code{package}.  
}

\seealso{\code{packageDescription}}

\author{ Olivia Lau <\email{olau@fas.harvard.edu}>
}

\examples{
\dontrun{
current.packages("Zelig")
}}

\keyword{file}
