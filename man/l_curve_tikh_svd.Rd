\name{l_curve_tikh_svd}
\alias{l_curve_tikh_svd}
\title{L-curve Tikhonov
}
\description{L-curve for Tikhonov regularization
}
\usage{
l_curve_tikh_svd(U, s, d, npoints, varargin = NULL)
}
\arguments{
  \item{U}{matrix of data space basis vectors from the svd

}
  \item{s}{vector of singular values
}
  \item{d}{the data vector
}
  \item{npoints}{the number of logarithmically spaced regularization parameters
}
\item{varargin}{
  alpha_min, alpha_max: if specified, constrain the logrithmically spaced
   regularization parameter range, otherwise an attempt is made to estimate
   them from the range of singular values

}
}
\details{Calculates the L-curve
}
\value{
\item{eta}{the solution norm ||m|| or seminorm ||Lm||}
  \item{rho}{the residual norm ||G m - d||}
  \item{reg_param}{corresponding regularization parameters}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
