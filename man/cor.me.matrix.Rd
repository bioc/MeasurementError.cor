\name{cor.me.matrix}
\alias{cor.me.matrix}
\title{ A function to calculate measurement error estimates for all pairs of genes given by the matrix }
\description{
Given a matrix ( p x n) for observed values of p variables and a corresponding matrix for their standard errors, the all pairwise measurement error estimates for true correlations are returned}
\usage{
cor.me.matrix(exp, se)
}
\arguments{
  \item{exp}{ observed value marix}
  \item{se}{ standard error matrix }
}
\details{
}
\value{
  The final estimates for true correlation (i.e. \code{cor.true}) from the measurement error model
}
\references{ Ding, B.Y. and Gentleman, R.(2003) Measurement error model for correlation coeffcient estimation and its application in microarray analsysis }

\author{ Beiying Ding}
\note{ The function involves using quasi-newton for linear optimization, "BFGS" is the only implemented method now. 
Refer to \code{cor.me.vector} for more details.}
\seealso{ cor.me.vector }
\examples{
 exp <- matrix(abs(rnorm(200,1000,20)),ncol=10)
 se <- matrix(abs(rnorm(200,50,5)),ncol=10)
 cor.me.matrix(exp,se)
}
\keyword{multivariate}

