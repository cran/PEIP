\name{blf2}
\alias{blf2}
\title{Bounded least squares
}
\description{Bounded least squares
}
\usage{
blf2(A, b, c, delta, l, u)
}
\arguments{
  \item{A}{Design Matrix
}
  \item{b}{Right hand side
}
  \item{c}{matrix weight on x
}
  \item{delta}{tolerance 
}
  \item{l}{lower bound
}
  \item{u}{upper bound
}
}
\details{
Solves the problem: min/max c'*x where || Ax-b || <= delta
and l <= x <= u.
}
\value{
  \item{x}{solution}
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic
  Press, Amsterdam, 2005.

  Stark, P.B. , and R. L. Parker, \emph{Bounded-Variable Least-Squares: An Algorithm
  and Applications}, Computational Statistics 10:129-141, 1995.

  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
