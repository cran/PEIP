\name{USV}
\alias{USV}
\title{Singular Value Decomposition
}
\description{Singular Value Decomposition
}
\usage{
USV(G)
}
\arguments{
  \item{G}{Matrix
}
}
\details{
  returns matrices U, S, V according to
  matlab convention.
}
\value{
  list:
  \item{U}{Matrix}
  \item{S}{Matrix, singular values}
  \item{V}{Matrix}
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{svd
}
\examples{


 hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
     X <- hilbert(9)[,1:6]
     
h = USV(X)

print( h$U )


}
\keyword{misc}
