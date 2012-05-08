\name{mcmc}
\alias{mcmc}
\title{Maximum likelihood Models 
}
\description{Maximum likelihood Models 
}
\usage{
mcmc(alogprior, aloglikelihood, agenerate, alogproposal, m0, niter)
}
\arguments{
  \item{alogprior}{Name of a function that computes the log of
    the prior distribution.
}
  \item{aloglikelihood}{Name of a function the computes the log of
                     the likelihood.

}
  \item{agenerate}{Name of a function that generates a random
  model from the current model using the

}
  \item{alogproposal}{Name of a function that computes the log of
the proposal distribution r(x,y).

}
  \item{m0}{Initial model
}
  \item{niter}{Number of iterations to perform
}
}

\value{
 \item{mout}{ MCMC samples}
 \item{mMAP}{Best model found in the MCMC simulation.}
 \item{accrate}{ Acceptance rate}

  
}


\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
