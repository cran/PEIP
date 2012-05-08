\name{wGSVD}
\alias{wGSVD}
\title{wrapper for GSVD
}
\description{wrapper for GSVD, dggsvd, generalized SVD
}
\usage{
wGSVD(A, B)
}
\arguments{
  \item{A}{matrix, A,A=U*E1*t(Q)
}
  \item{B}{matrix, V*E2*t(Q)
}
}
\details{Uses LAPACK routine dggsvd
}
\value{list:
  U,V, X, C,S
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{This function calls the LAPACK routine installed
  on the system.  For LINUX, MAC and WINDOWS there may be
  slightly different implementation.
  
  The order of the singular values returned are
  according to Matlab convention, smallest to largest.
}


\seealso{svd
}

\keyword{misc}
