\name{phiinv}
\alias{phiinv}
\title{Inverse Normal Distribution Integral
}
\description{
   Calculates the inverse normal distribution from the value of the
 integral

}
\usage{
phiinv(x)
}
\arguments{
  \item{x}{endpoint value of integration (scalar)

}
}

\value{value of integral (scalar)

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{phi
}

\examples{

 x <- 1.0
##   pracma::erf(x)
 phi(x)
phiinv( phi(x) )


}

\keyword{misc}
