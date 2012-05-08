\name{get_l_rough}
\alias{get_l_rough}
\title{One-D Roughening
}
\description{
  returns a 1D differentiating matrix operating on a series with n points.
}
\usage{
get_l_rough(n, deg)
}
\arguments{
  \item{n}{number of data points

}
  \item{deg}{order of the derivative to approximate
}
}
\details{Used to get first and 2nd order roughening matrices
}
\value{Matrix:discrete differentiation matrix 
}
\references{ Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
