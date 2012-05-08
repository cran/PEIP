\name{irls}
\alias{irls}
\title{Iteratively reweight least squares
}
\description{
   Uses the iteratively reweight least squares strategy to find an
 approximate L_p solution to Ax=b.

}
\usage{
irls(A, b, tolr, tolx, p, maxiter)
}
\arguments{
  \item{A}{Matrix of the system of equations.
}
  \item{b}{Right hand side of the system of equations
}
  \item{tolr}{Tolerance below which residuals are ignored
}
  \item{tolx}{Stopping tolerance.  Stop when (norm(newx-x)/(1+norm(x)) < tolx)
}
  \item{p}{Specifies which p-norm to use (most often, p=1.)
}
  \item{maxiter}{Limit on number of iterations of IRLS
}
}
\details{
  Use to get L-1 norm solution of inverse problems.
}
\value{
  \item{x}{ Approximate L_p solution}
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
