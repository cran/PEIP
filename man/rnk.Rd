\name{rnk}
\alias{rnk}
\title{Rank of Matrix
}
\description{Return the rank of a matrix.  Not to be confused with the
  R function rank.
  
}
\usage{
rnk(G, tol = 1e-14)
}
\arguments{
  \item{G}{Matrix
}
  \item{tol}{machine tolerance for small numbers
}
}
\details{Number of singular values greater than
  tol.
}
\value{integer, number of non-zero singular values
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{duplicate the matlab function rank
}


\seealso{svd
}
\examples{


hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
X <- hilbert(9)[,1:6]
rnk(X)


}
\keyword{misc}
