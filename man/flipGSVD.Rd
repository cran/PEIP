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
\details{This Flipping of the matrix
  is done to agree with the Matlab code.
}
\value{
  
  \item{ U}{}
   \item{V}{}
   \item{X}{}
   \item{C}{}
   \item{S}{}

  
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{ The GSVD routines are from LAPACK.
}


\seealso{GSVD
}

\keyword{misc}
