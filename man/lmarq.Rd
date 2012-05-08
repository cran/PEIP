\name{lmarq}
\alias{lmarq}
\title{Lev-Marquardt Inversion
}
\description{
  Use the Levenberg-Marquardt algorithm to minimize
  f(p)=sum(F_i(p)^2)

}
\usage{
lmarq(afun, ajac, p0, tol, maxiter)
}
\arguments{
  \item{afun}{name of the function F(x)

}
  \item{ajac}{ name of the Jacobian function J(x)
}
  \item{p0}{initial guess
}
  \item{tol}{stopping tolerance
}
  \item{maxiter}{maximum number of iterations allowed
}
}

\value{
  \item{pstar}{        best solution found.}
 \item{iter}{         Iteration count.}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
