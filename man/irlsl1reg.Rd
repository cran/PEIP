\name{irlsl1reg}
\alias{irlsl1reg}
\title{L1 least squares with sparsity
}
\description{
  Solves the system Gm=d using sparsity regularization on Lm.
  Solves the L1 regularized least squares problem:
  min norm(G*m-d,2)^2+alpha*norm(L*m,1)
}
\usage{
irlsl1reg(G, d, L, alpha, maxiter = 100, tolx = 1e-04, tolr = 1e-06)
}
\arguments{
  \item{G}{design matrix
}
  \item{d}{right hand side
}
  \item{L}{regularization matrix
}
  \item{alpha}{regularization parameter
}
  \item{maxiter}{Maximum number of IRLS iterations
}
  \item{tolx}{Tolerance on successive iterates
}
  \item{tolr}{Tolerance below which we consider an element of L*m to be
    effectively zero
}
}

\value{
  \item{m}{model vector}
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


n = 20
G = shawG(n,n)

spike = rep(0,n)
spike[10] = 1

spiken = G \%*\%  spike

wts = rep(1, n)
delta = 1e-03
set.seed(2015)
dspiken = spiken + 6e-6 *rnorm(length(spiken))
L1 = get_l_rough(n,1);
alpha = 0.001

k = irlsl1reg(G, dspiken, L1, alpha, maxiter = 100, tolx = 1e-04, tolr = 1e-06)


plotconst(k,-pi/2,pi/2, ylim=c(-.2,  0.5), xlab="theta", ylab="Intensity" );


}




\keyword{misc}
