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

\keyword{misc}
