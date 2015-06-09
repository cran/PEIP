\name{blf2}
\alias{blf2}
\title{Bounded least squares
}
\description{Bounded least squares }
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

\examples{

###  set up an inverse problem:Shaw problem

n = 20
G = shawG(n,n)

spike = rep(0,n)
spike[10] = 1

spiken = G \%*\%  spike

wts = rep(1, n)
delta = 1e-03
set.seed(2015)
dspiken = spiken + 6e-6 *rnorm(length(spiken))

lb = spike - (.2) * wts
ub = spike + (.2) * wts

dspiken = dspiken

 blf2(G, dspiken, wts , delta, lb, ub)


}

\keyword{misc}
