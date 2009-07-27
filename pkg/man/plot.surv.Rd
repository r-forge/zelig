\name{plot.surv}

\alias{plot.surv}

\title{Plotting Confidence Intervals for Survival Curves}

\description{ The \code{plot.surv} command generates confidence intervals for Kaplan-Meier survival curves}

\usage{
     plot.surv(x, duration, censor, type = "line", plotcensor=TRUE,
               plottimes = FALSE, int = c(0.025,0.975), ...) 
}

\arguments{
\item{x}{output from \code{sim} stored as a list.  Each element of the list is the \code{sim} output for a particular survival curve.}  
\item{duration}{the duration variable (e.g. lifetime, survival, etc.).}
\item{censor}{the censored data}
\item{type}{the type of confidence interval.  Defaults to \code{"line"}, which draws vertical confidence intervals at observed event times.  \code{"poly"} draws confidence regions using polygons.}
\item{plotcensor}{default is \code{TRUE}. Plots censoring times as a \code{rug} object.}
\item{plottimes}{default is \code{FALSE}. Plots step function with indicators at observed event times.}
\item{int}{vector of quantile limits for the confidence interval.  Default is 95\% interval.}
\item{\dots}{Additional parameters passed to \code{plot}.}
}  

\value{
For survival models, \code{plot.surv()} returns vertical
confidence intervals or polygon survival regions for Kaplan-Meier survival curves.  You may save this plot using the commands described in the
Zelig manual (\url{http://gking.harvard.edu/zelig}).  
}

\examples{\dontrun{
data(coalition)
z.out1 <- zelig(Surv(duration,ciep12)~invest+numst2+crisis,
robust=TRUE,cluster="polar",model="coxph",data=coalition)
low <- setx(z.out1,numst2=0)
high <- setx(z.out1,numst2=1
# Simulate Survival Curves for Each Group
s.out1 <- sim(z.out1,x=low) 
s.out2 <- sim(z.out1,x=high)

# Organize simulated output as a list
out <- list(s.out1,s.out2)

plot.surv(x = out, duration = coalition$duration, censor=coalition$ciep12,
          type="line", plottimes=FALSE, plotcensor=FALSE,
          main="Survival", xlab="Time", ylab="Survival")
}
}


\seealso{
  The full Zelig manual is available at
  \url{http://gking.harvard.edu/zelig}, and users may also wish to see
  \code{plot}, \code{lines}.
  }

\author{
  John A. Graves <\email{graveja0@gmail.com}>
}

\keyword{hplot}





