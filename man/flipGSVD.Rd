\name{flipGSVD}
\alias{flipGSVD}
\title{Flip output of GSVD
}
\description{Flip (reverse order) output of GSVD
}
\usage{
flipGSVD(vs, d1 = c(50, 50), d2 = c(48, 50))
}
\arguments{
  \item{vs}{list output of GSVD
}
  \item{d1}{dimensionals of A
}
  \item{d2}{dimensions of B
}
}
\details{This flipping of the matrix
  is done to agree with the Matlab code.
}
\value{
  Same as GSVD, but order of eigenvectors is reversed.
    \item{U}{m by m orthogonal matrix}
  \item{V}{p by p orthogonal matrix, p=rank(B)}
  \item{X}{n by n nonsingular matrix}
  \item{C}{singular values, m by n matrix with diagonal elements shifted from main diagonal}
  \item{S}{singular values, p by n diagonal matrix}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{ The GSVD routines are from LAPACK.
}

\examples{

set.seed(12)

n <- 5
A <- matrix(runif(n*n),nrow=n)
B <- matrix(runif(n*n),nrow=n)

VS = GSVD(A, B)

FVS = flipGSVD(VS, d1 = dim(A) , d2 = dim(B) )
## see that order of eigen vectors is reversed
diag(VS$S)
diag(FVS$S)

}
\seealso{GSVD
}

\keyword{misc}
