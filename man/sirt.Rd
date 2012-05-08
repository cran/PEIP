\name{sirt}
\alias{sirt}
\title{SIRT Algorithm for sparse matrix inversion
}
\description{
  Row action method for inversion of matrices
}
\usage{
sirt(A, b, tolx, maxiter)
}
\arguments{
  \item{A}{Design Matrix
}
  \item{b}{vector, Right hand side
}
  \item{tolx}{numeric, tolerance for stopping
}
  \item{maxiter}{integer, Maximum iterations
}
}
\details{Iterates until conversion
}
\value{Solution vector
}
\references{
  See Lees, 1989
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{art, kac
}

\keyword{misc}
