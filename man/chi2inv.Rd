\name{chi2inv}
\alias{chi2inv}
\title{Inverse Chi-Sq
}
\description{Inverse Chi-Sq
}
\usage{
chi2inv(x, n)
}
\arguments{
  \item{x}{probability that Chi^2 random variable is less than or equal to x (scalar).

}
  \item{n}{degrees of freedom(scalar)

}
}
\details{
   Computes the inverse Chi^2 distribution corresponding to a given
 probability that a Chi^2 random variable with the given degrees
 of freedom is less than or equal to x.  Uses chi2cdf.m.

}
\value{corresponding value of x for given probability.

}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{chi, chi2cdf
}

\examples{
x = seq(from=0.1, to=0.9, length=10) 
h = chi2cdf(x, 3)

chi2inv(h, 3)

}

\keyword{misc}
