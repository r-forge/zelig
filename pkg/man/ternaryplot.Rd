\name{ternaryplot}
\alias{ternaryplot}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Ternary diagram}
\description{
Visualizes compositional, 3-dimensional data in an equilateral triangle  
(from the vcd library, Version 0.1-3.3, Date 2004-04-21), using plot graphics.  
Differs from implementation in vcd (0.9-7), which uses grid graphics.}
\usage{
ternaryplot(x, scale = 1, dimnames = NULL, dimnames.position = c("corner","edge","none"),
            dimnames.color = "black", id = NULL, id.color = "black", coordinates = FALSE,
	    grid = TRUE, grid.color = "gray", labels = c("inside", "outside", "none"),
	    labels.color = "darkgray", border = "black", bg = "white", pch = 19, cex = 1,
	    prop.size = FALSE, col = "red", main = "ternary plot", ...)
}
\arguments{
  \item{x}{a matrix with three columns.}
  \item{scale}{row sums scale to be used.}
  \item{dimnames}{dimension labels (defaults to the column names of
    \code{x}).}
  \item{dimnames.position, dimnames.color}{position and color of dimension labels.}
  \item{id}{optional labels to be plotted below the plot
    symbols. \code{coordinates} and \code{id} are mutual exclusive.}
  \item{id.color}{color of these labels.}
  \item{coordinates}{if \code{TRUE}, the coordinates of the points are
    plotted below them. \code{coordinates} and \code{id} are mutual exclusive.}
  \item{grid}{if \code{TRUE}, a grid is plotted. May optionally
    be a string indicating the line type (default: \code{"dotted"}).}
  \item{grid.color}{grid color.}
  \item{labels, labels.color}{position and color of the grid labels.}
  \item{border}{color of the triangle border.}
  \item{bg}{triangle background.}
  \item{pch}{plotting character. Defaults to filled dots.}
  \item{cex}{a numerical value giving the amount by which plotting text
    and symbols should be scaled relative to the default. Ignored for
    the symbol size if \code{prop.size} is not \code{FALSE}.}
  \item{prop.size}{if \code{TRUE}, the symbol size is plotted
    proportional to the row sum of the three variables, i.e. represents
    the weight of the observation.}
  \item{col}{plotting color.}
  \item{main}{main title.}
  \item{\dots}{additional graphics parameters (see \code{par})}
}
\details{
A points' coordinates are found by computing the gravity center
of mass points using the data entries as weights. Thus, the coordinates
of a point P(a,b,c), \eqn{a + b + c = 1}, are: P(b + c/2, c * sqrt(3)/2).
}

\examples{
data(mexico)
if (require(VGAM)) { 
z.out <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, 
                model = "mlogit", data = mexico)
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)

ternaryplot(s.out$qi$ev, pch = ".", col = "blue",
            main = "1988 Mexican Presidential Election")
}
}

\seealso{\code{\link{ternarypoints}}}

\references{
M. Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}

\keyword{hplot}
