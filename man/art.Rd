\name{art}
\alias{art}
\title{ART Inverse solution
}
\description{ART algorythm for solving
  sparse linear inverse problems
}
\usage{
art(A, b, tolx, maxiter)
}
\arguments{
  \item{A}{Constraint matrix
}
  \item{b}{right hand side
}
  \item{tolx}{difference tolerance for successive iterations (stopping criteria)
}
  \item{maxiter}{ maximum iterations (stopping criteria).
}
}
\details{
  Alpha is a damping factor.  If alpha<1, then we won't take full steps
 in the ART direction.  Using a smaller value of alpha (say alpha=.75)
 can help with convergence on some problems.

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
