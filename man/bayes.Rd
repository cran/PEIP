\name{bayes}
\alias{bayes}
\title{Bayes Inversion
}
\description{
  Given a linear inverse problem Gm=d, a prior mean mprior and covariance
  matrix covm, data d, and data covariance matrix covd, this function
  computes the MAP solution and the corresponding covariance matrix. 
}
\usage{
bayes(G, mprior, covm, d, covd)
}
\arguments{
  \item{G}{Design Matrix
}
  \item{mprior}{vector, prior model
}
  \item{covm}{vector, model covariance
}
  \item{d}{vector, right hand side
}
  \item{covd}{vector, data covariance
}
}

\value{vector model
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
