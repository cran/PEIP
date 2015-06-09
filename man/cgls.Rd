\name{cgls}
\alias{cgls}
\title{Conjugate gradient Least squares 
}
\description{Conjugate gradient Least squares 
}
\usage{
cgls(Gmat, dee, niter)
}
\arguments{
  \item{Gmat}{input matrix
}
  \item{dee}{right hand side
}
  \item{niter}{max number of iterations
}
}
\details{
   Performs niter iterations of the CGLS algorithm on the least
 squares problem  min norm(G*m-d).  Gmat should be a sparse matrix.


}
\value{
  \item{X}{matrix of  models}
  \item{rho}{misfit norms}
  \item{eta}{model norms}

  
  
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
set.seed(11)
####  perfect data with no noise
n <- 5
A <- matrix(runif(n*n),nrow=n)
B <- runif(n)
###  get right-hand-side (data)
trhs = as.vector( A \%*\% B  )
Lout = cgls(A, trhs , 15)

###  solution is
Lout$X[,15]

Lout$X[,15] - B



}

\keyword{misc}
