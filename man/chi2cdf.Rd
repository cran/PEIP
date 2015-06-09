\name{chi2cdf}
\alias{chi2cdf}
\title{Chi-Sq CDF
}
\description{Computes the Chi^2 CDF, using a transformation to N(0,1) on page
 333 of Thistead, Elements of Statistical Computing.

  
}
\usage{
chi2cdf(x, n)
}
\arguments{
  \item{x}{end value of chi^2 pdf to integrate to. (scalar)

}
  \item{n}{degrees of freedom (scalar)

}
}
\details{Note that x and m must be scalars.

}
\value{
  \item{p}{ probability that Chi^2 random variable is less than or equal to x (scalar).}

}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
x= seq(from=0.1, to=0.9, length=20)
chi2cdf(x , 3)

}

\keyword{misc}
