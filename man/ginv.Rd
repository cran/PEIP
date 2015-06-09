\name{ginv}
\alias{ginv}
\title{Get inverse
}
\description{
  Get inverse of matrisx or solve Ax=b.
}
\usage{
ginv(G, x, tol = 1e-12)
}
\arguments{
  \item{G}{Design Matrix
}
  \item{x}{right hand side
}
  \item{tol}{tolerance
}
}
\details{
  This function used as alternative to matlab code that does this h = G\\x to get the inverse
}
\value{inverse as a N by 1 matrix.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  Be careful about the usage of tolerance
}

\examples{
set.seed(2015)
GAB = matrix(runif(36), ncol=6)
truex =rnorm(ncol(GAB))
rhs = GAB \%*\% truex

rhs = as.vector(rhs )

tout = ginv(GAB, rhs, tol = 1e-12)
tout - truex 

}


\seealso{solve, Ainv 
}

\keyword{misc}
