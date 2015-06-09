\name{dcost}
\alias{dcost}
\title{cosine transform
}
\description{Computes the column-by-column discrete cosine transform of X.

}
\usage{
dcost(X)
}
\arguments{
  \item{X}{Time series matrix
}
}

\value{cosine transformaed data
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\examples{

 x <- 1:4

### compare fft with cosine transform
     fft(x)
    
dcost(x)

}



\keyword{misc}
