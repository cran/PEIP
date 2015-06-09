\name{get_l_rough}
\alias{get_l_rough}
\title{One-D Roughening
}
\description{
  Returns a 1D differentiating matrix operating on a series with n points.
}
\usage{
get_l_rough(n, deg)
}
\arguments{
  \item{n}{integer, number of data points

}
  \item{deg}{order of the derivative to approximate
}
}
\details{Used to get first and 2nd order roughening matrices for 1-D problems
}
\value{Matrix:discrete differentiation matrix 
}
\references{ Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

### first order roughening matrix for a 10 by 10 model: a sparse matrix
N = 10
L1 = get_l_rough(10,1);

### second order roughening matrix for a 10 by 10 model
N = 10
L2 = get_l_rough(10,2);

}



\keyword{misc}
