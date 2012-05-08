\name{l_curve_corner}
\alias{l_curve_corner}
\title{L Curve Corner
}
\description{Retrieve corner of L-curve
}
\usage{
l_curve_corner(rho, eta, reg_param)
}
\arguments{
  \item{rho}{misfit
}
  \item{eta}{model norm or seminorm

}
  \item{reg_param}{regularization parameter
}
}

\value{
    \item{reg_corner}{   the value of reg_param with maximum curvature}
    \item{ireg_corner}{  the index of the value in reg_param with maximum curvature}
    \item{kappa}{        the curvature for each reg_param}

}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
