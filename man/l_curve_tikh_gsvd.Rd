\name{l_curve_tikh_gsvd}
\alias{l_curve_tikh_gsvd}
\title{L-curve tikh gsvd
}
\description{L-curve tikh gsvd
}
\usage{
l_curve_tikh_gsvd(U, d, X, Lam, Mu, G, L, npoints, varargin = NULL)
}
\arguments{
  \item{U}{from the gsvd 
}
  \item{d}{data vector for the problem G*m=d
}
  \item{X}{from the gsvd 
}
  \item{Lam}{from the gsvd 
}
  \item{Mu}{from the gsvd 
}
  \item{G}{system matrix 
}
  \item{L}{roughening matrix
}
  \item{npoints}{
}
\item{varargin}{
alpha_min, alpha_max: if specified, constrain the logrithmically spaced
 regularization parameter range, otherwise an attempt is made to estimate
 them from the range of generalized singular values

  
}
}
\details{Uses output of GSVD
}
\value{
   \item{eta}{       - the solution seminorm ||Lm||}
   \item{rho}{       - the residual norm ||G m - d||}
   \item{reg_param}{ - corresponding regularization parameters}
   \item{m}{         - corresponding suite of models for truncated GSVD}


  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
