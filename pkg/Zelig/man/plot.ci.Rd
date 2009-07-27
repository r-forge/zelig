\name{plot.ci}

\alias{plot.ci}

\title{Plotting Vertical confidence Intervals}

\description{ The \code{plot.ci} command generates vertical
  confidence intervals for linear or generalized linear univariate
  response models.  }

\usage{
plot.ci(x, CI = 95, qi = "ev", main = "", ylab = NULL, xlab = NULL,
        xlim = NULL, ylim = NULL, col = c("red", "blue"), ...) 
}

\arguments{
  \item{x}{stored output from \code{sim}.  The \code{x$x} and optional
  \code{x$x1} values used to generate the \code{sim} output object must
  have more than one observation.}  
\item{CI}{the selected confidence interval.  Defaults to 95
  percent.}
\item{qi}{the selected quantity of interest.  Defaults to
  expected values.}
\item{main}{a title for the plot.}
\item{ylab}{label for the y-axis.}
\item{xlab}{label for the x-axis.}
\item{xlim}{limits on the x-axis.}
\item{ylim}{limits on the y-axis.}
\item{col}{a vector of at most two colors for plotting the
  expected value given by \code{x} and the alternative set of expected
  values given by \code{x1} in \code{sim}.  If the quantity of
  interest selected is not the expected value, or \code{x1 = NULL},
  only the first color will be used.}
\item{\dots}{Additional parameters passed to \code{plot}.}
}  

\value{
For all univariate response models, \code{plot.ci()} returns vertical
confidence intervals over a specified range of one explanatory
variable.  You may save this plot using the commands described in the
Zelig manual (\url{http://gking.harvard.edu/zelig}).  
}

\examples{
data(turnout)
z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "logit", data = turnout)
age.range <- 18:95
x.low <- setx(z.out, educate = 12, age = age.range)
x.high <- setx(z.out, educate = 16, age = age.range)
s.out <- sim(z.out, x = x.low, x1 = x.high)
plot.ci(s.out, xlab = "Age in Years",
        ylab = "Predicted Probability of Voting",
        main = "Effect of Education and Age on Voting Behavior")
legend(45, 0.52, legend = c("College Education (16 years)",
       "High School Education (12 years)"), col = c("blue","red"), 
       lty = c("solid"))
}

\seealso{
  The full Zelig manual is available at
  \url{http://gking.harvard.edu/zelig}, and users may also wish to see
  \code{plot}, \code{lines}.
  }

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{hplot}





