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

\examples{
t = 1:10
y=c(109.3827,187.5385,267.5319,331.8753,386.0535,
428.4271,452.1644,498.1461,512.3499,512.9753)
sigma = rep(8, length(y))
N=length(t);

### % Introduce the outlier
y[4]=y[4]-200;

G = cbind( rep(1, N), t, -1/2*t^2 )

### % Apply the weighting

yw = y/sigma;

Gw = G/sigma

m2 = solve( t(Gw) \%*\% Gw , t(Gw) \%*\% yw, tol=1e-12 )


###  Solve for the 1-norm solution

m1 = irls(Gw,yw,1.0e-5,1.0e-5,1,25)
m1



}

\keyword{misc}
