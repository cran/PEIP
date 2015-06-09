\name{Ainv}
\alias{Ainv}
\title{An Inverse Solution
}
\description{QR decomposition solution to Ax=b
}
\usage{
Ainv(GAB, x, tol = 1e-12)
}
\arguments{
  \item{GAB}{design matrix
}
  \item{x}{right hand side
}
  \item{tol}{tolerance for singularity
}
}
\details{
  I needed something to make up for the lame-o matlab code that does this h = G\\x to get the inverse
}
\value{
  Inverse Solution
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\examples{
set.seed(2015)
GAB = matrix(runif(36), ncol=6)
truex =rnorm(ncol(GAB))
rhs = GAB \%*\% truex

rhs = as.vector(rhs )

tout = Ainv(GAB, rhs, tol = 1e-12)
tout - truex 

}

\keyword{misc}
