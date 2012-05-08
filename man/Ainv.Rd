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
  need something to make up for the lame-o matlab code that does this h = G\\x to get the inverse
}
\value{
  Inverse Solution
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
