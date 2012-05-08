\name{kac}
\alias{kac}
\title{Kaczmarz
}
\description{
  Implements Kaczmarz's algorithm to solve a system of equations iteratively
}
\usage{
kac(A, b, tolx, maxiter)
}
\arguments{
  \item{A}{Constraint matrix
}
  \item{b}{right hand side
}
  \item{tolx}{difference tolerence for successive iterations (stopping criteria)
}
  \item{maxiter}{maximum iterations (stopping criteria)
}
}

\value{
  \item{x}{solution}
  
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
