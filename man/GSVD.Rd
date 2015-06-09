\name{GSVD}
\alias{GSVD}
\title{Generalized SVD
}
\description{Wrapper for generalized svd from LAPACK
}
\usage{
GSVD(A, B)
}
\arguments{
  \item{A}{U*C*t(X), see below }
  \item{B}{V*S*t(X), see below }
}

\value{
  \item{U}{m by m orthogonal matrix}
  \item{V}{p by p orthogonal matrix, p=rank(B)}
  \item{X}{n by n nonsingular matrix}
  \item{C}{singular values, m by n matrix with diagonal elements shifted from main diagonal}
  \item{S}{singular values, p by n diagonal matrix}
}
\details{
Since PEIP is based on a book, which is iteslef based on MATLAB routines,
the convention here follows the book.  The R implementation uses LAPACK
and wraps the function so the output will comply with the book.  See page
104 of the second edition of the Aster book cited below.  That said,
the purpose is to find an inversion of the form Y = t(A aB),
where a is a regularization parameter, B is
smoothing matrix and A is the design matrix for the forward problem.
The input matrices A and B are assumed to have full rank, and
p = rank(B).  The generalized singular values are then gamma = lambda/mu,
where lambda = sqrt(diag(t(C)*C) ) and mu = sqrt(diag(t(S)*S) ).
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems},
  Elsevier Academic Press, Amsterdam, 2005.
}
\seealso{flipGSVD}

\examples{
set.seed(11)

n <- 5
A <- matrix(runif(n*n),nrow=n)
B <- matrix(runif(n*n),nrow=n)

g = GSVD(A, B)

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Requires R version of LAPACK. The code is a wrapper
for the dggsvd function in LAPACK.  The author thanks Berend Hasselman for advice
and help preparing this function.
}

\keyword{misc}
