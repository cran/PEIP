\name{Mnorm}
\alias{Mnorm}
\title{Matrix Norm
}
\description{Matrix Norm
}
\usage{
Mnorm(X, k = 2)
}
\arguments{
  \item{X}{matrix
}
  \item{k}{norm number
}
}
\details{
 returns the largest singular value of the matrix or vector 
}
\value{
  Scalar Norm
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{if k=1, absolute value; k=2 2-norm (rms); k>2, largest singular value.
}


\keyword{misc}
