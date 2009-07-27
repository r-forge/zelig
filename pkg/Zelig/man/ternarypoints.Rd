\name{ternarypoints}

\alias{ternarypoints}

\title{Adding Points to Ternary Diagrams}

\description{
Use \code{ternarypoints} to add points to a ternary diagram generated
using the \code{ternaryplot} function in the vcd library.  Use
ternary diagrams to plot expected values for multinomial choice models
with three categories in the dependent variable.  
}

\usage{
ternarypoints(object, pch = 19, col = "blue", ...)
}

\arguments{
\item{object}{The input object must be a matrix with three
  columns. }
\item{pch}{The selected type of point.  By default, \code{pch =
    19}, solid disks. } 
\item{col}{The color of the points.  By default, \code{col =
    "blue"}. }  
\item{\dots}{Additional parameters passed to \code{points}. }  
}

\value{
The \code{ternarypoints} command adds points to a previously existing
ternary diagram.  Use \code{ternaryplot} in the \code{vcd} library to
generate the main ternary diagram.  
}

\seealso{ The full Zelig manual at
  \url{http://gking.harvard.edu/zelig}, \code{points}, and
  \code{\link{ternaryplot}}.  }

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{aplot}





