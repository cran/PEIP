\name{occam}
\alias{occam}
\title{Occam inversion
}
\description{Occam's inversion
}
\usage{
occam(afun, ajac, L, d, m0, delta)
}
\arguments{
  \item{afun}{character, function handle that computes the forward problem
}
  \item{ajac}{character, function handle that computes the Jacobian of the forward problem
}
  \item{L}{regularization matrix
}
  \item{d}{data that should be fit
}
  \item{m0}{guess at the model
}
  \item{delta}{cutoff to use for the discrepancy principle portion
}
}

\value{vector, model found
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  This is a simple brute force way to do the line search.  Much more
   sophisticated methods are available.  Note: we've restricted the line
   search to the range from 1.0e-20 to 1.  This seems to work well in
   practice, but might need to be adjusted for a particular problem.

  
}


\seealso{bayes
}

\keyword{misc}
