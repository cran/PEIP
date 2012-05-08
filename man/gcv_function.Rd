\name{gcv_function}
\alias{gcv_function}
\title{gcv func
}
\description{
  Auxiliary routine for GCV calculations
}
\usage{
gcv_function(alpha, gamma2, beta)
}
\arguments{
  \item{alpha}{parameter
}
  \item{gamma2}{square of the gamma from the gsvd
}
  \item{beta}{projected data to fit
}
}

\value{
  vector, g - || Gm_(alpha,L) - d ||^2 / (Tr(I - GG#)^2
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
