\name{cor.me.vector}
\alias{cor.me.vector}
\title{ A function to calculate measurement error model estimates for correlation coefficient betweem two variables }
\description{
Given the observed value of two variables and their respective standard error, the measurement error estimate for their correlation coefficient is returned}
\usage{
cor.me.vector(exp1, se1, exp2, se2)
}
\arguments{
  \item{exp1}{ observed value for vector 1 }
  \item{se1}{ estimated standard error for vector 1 }
  \item{exp2}{ observed value for vector 2 }
  \item{se2}{ estimated standard error for vector 2 }
}
\details{
}
\value{
  \item{estimate }{Vecotr containing the estimates from the measurement error model, i.e. \code{cor.me}, \code{cor.true}, \code{mu1}, \code{mu2}, \code{s1}, \code{s2} which are correlation for the measurement error distribution of the two variables, true correlation between the two variables, and true mean and standard deviation estimates for the two variables respectively}
  \item{count }{numer of function and gradient evaluation }
  \item{convergence}{ 0 if converged.  See optim() for details}
}

\references{ Ding, B.Y. and Gentleman, R. (2003) Measurement Error Model for correlation coefficient estimation and its application in microarray analysis }

\author{ Beiying Ding}
\note{ 
Most applicable for microarray expression data where standard errors are readily estimated by most low level analysis softwares. Hence variables can be thought of as genes. One also need to differentiate between cor.me and cor.true: the first one being the correlation between the measurement error distributions of the two genes whereas the second one is the quantity of interest, i.e true correlation between the two gene expression profiles.\\

The function involves using quasi-newton for linear optimization, "BFGS" is the only implemented method now. }
\seealso{ \code{cor.me.matrix} }
\examples{
 exp <- matrix(abs(rnorm(200,1000,20)),ncol=10)
 se <- matrix(abs(rnorm(200,50,5)),ncol=10)
 cor.me.vector(exp[1,],se[1,],exp[2,],se[2,])
}
\keyword{multivariate}

