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

\examples{
fun <-function(m,x)
{
  y=m[1]*exp(m[2]*x)+m[3]*x*exp(m[4]*x)
  return(y)
}

generate <-function( x) {  
  y=x+step*rnorm(4)
  return(y)
}

logprior <-function(m)
{
  if( (m[1]>=0) & (m[1]<=2) &
     (m[2]>=-0.9) & (m[2]<=0) &
     (m[3]>=0) & (m[3]<=2) &
     (m[4]>=-0.9) & (m[4]<=0)  )
    {
      lp=0
    }
  else
    {
      lp= -Inf
    }

  return(lp)
}
loglikelihood <-function(m)
{ 
  fvec=(y-fun(m,x))/sigma
  L=(-1/2)*sum(fvec^2)
  return(L)
}
logproposal <-function(x,y)
  {  
    LR=(-1/2)*sum((x-y)^2/step^2)
    return(LR)
  }

###  Generate the data set.
x=seq(from=1, by=0.25, to=7.0)

mtrue=c(1.0, -0.5, 1.0, -0.75)

ytrue=fun(mtrue,x)

sigma=0.01*rep(1, times= length(ytrue) )

y=ytrue+sigma*rnorm(length(ytrue) )

### set the MCMC parameters
### number of skips to reduce autocorrelation of models
skip=100
### burn-in steps
BURNIN=1000
### number of posterior distribution samples
N=4100
### MVN step size
step = 0.005*rep(1,times=4)

###  We assume flat priors here
m0 = c(0.9003,
   -0.5377,
    0.2137,
   -0.0280)

alogprior='logprior'
aloglikelihood='loglikelihood'
agenerate='generate'
alogproposal='logproposal'

### ###  initialize model at a random point on [-1,1]

###  m0=(runif(4)-0.5)*2
###  this is the matlab initialization:
m0 = c(0.9003,
   -0.5377,
    0.2137,
   -0.0280)

MM =  mcmc('logprior','loglikelihood','generate','logproposal',m0,N)

mout = MM[[1]]
mMAP= MM[[2]]
pacc= MM[[3]]


}
\keyword{misc}
