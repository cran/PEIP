\name{phi}
\alias{phi}
\title{Integral of Normal Distribution
}
\description{normal distribution and returns the value of the integral

}
\usage{
phi(x)
}
\arguments{
  \item{x}{endpoint of integration (scalar)

}
}

\value{
  value of integral

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{erf
}

\examples{

 x <- 1.0
##   pracma::erf(x)
 phi(x)
phiinv( phi(x) )


}

\keyword{misc}
