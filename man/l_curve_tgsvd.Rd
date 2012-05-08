\name{l_curve_tgsvd}
\alias{l_curve_tgsvd}
\title{L curve tgsvd
}
\description{
  L curve parematers and models for truncated gsvd regularization.
}
\usage{
l_curve_tgsvd(U, d, X, Lam, G, L)
}
\arguments{
  \item{U}{U, output of GSVD
}
  \item{d}{ output of GSVD
}
  \item{X}{output of GSVD
}
  \item{Lam}{output of GSVD
}
  \item{G}{output of GSVD
}
  \item{L}{output of GSVD
}
}

\value{
List:
  \item{eta}{        the solution seminorm ||Lm||}
  \item{rho }{       the residual norm ||G m - d||}
  \item{reg_param}{  corresponding regularization parameters}
  \item{m}{          corresponding suite of models for truncated GSVD}


  
}
\references{ Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
