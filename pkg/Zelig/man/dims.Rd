\name{dims}

\alias{dims}

\title{Return Dimensions of Vectors, Arrays, and Data Frames}

\description{
  Retrieve the dimensions of a vector, array, or data frame.  
}

\usage{
dims(x)
}

\arguments{
  \item{x}{An R object.  For example, a vector, matrix, array, or data 
frame.}
}

\value{
  The function \code{dims} performs exactly the same as \code{dim}, and 
additionally returns the \code{length} of vectors (treating them as 
one-dimensional arrays).}

\seealso{\code{dim}, \code{length}}

\author{ Olivia Lau <\email{olau@fas.harvard.edu}>
}

\examples{
a <- 1:12
dims(a)

a <- matrix(1, nrow = 4, ncol = 9)
dims(a)
}

\keyword{file}
