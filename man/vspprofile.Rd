\name{vspprofile}
\alias{vspprofile}
\title{Vertical Seismic Profile In 1D
}
\description{Example vertical 1-dimensional seismic profile
used for setting up examples for inverse theory.
}
\usage{
vspprofile(M = 50, N = 50, maxdepth = 1000, deltobs = 20,
noise = 2e-04, M1 = c(9000, -6, 0.001))
}
\arguments{
  \item{M}{integer, number of rows in in design matrix G, default=50
}
  \item{N}{integer, number of columns in design matrix G, default=50
}
  \item{maxdepth}{Maximum depth of model, default = 1000
}
  \item{deltobs}{integer, sampling interval in depth, default=20
}
  \item{noise}{gausian noise multiplier, default=2e-04
}
  \item{M1}{3-vector, linear model for velocity versus depth model
}
}
\details{Vertical seismic profile in 1D dimension used for setting up examples in PEIP.
Given a simple velocity profile, defined by input parameter M1
create the travel times and designe matrix used for solving an inverse problem. The
velocity model is defined as depth versus velocity, and the function inverts that
from the slowness.  Any model could be used to replace this model.  The default model
here is taken from an inversion in the Aster book.
}
\value{
list:
\item{G}{M by N design matrix}
\item{tee}{true travel times from model}
\item{t2}{travel times with noise added}
\item{depth}{depth samples of model}
\item{vee}{velocity at the depths indicated}
 \item{M}{input M}
  \item{N}{input N}
  \item{maxdepth}{input maxdepth}
  \item{deltobs}{input delta observation}
  \item{noise}{input noise}
  \item{M1}{True model used for depth versus velocity}
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

V = vspprofile()
### plot quadratic velocity profile
plot(V$vee, -V$depth, main="VSP: velocity increasing with depth")
dobs = seq(from=V$deltobs, to=V$maxdepth, by=V$deltobs)
### plotdepth versus time (not linear)
plot(dobs, V$t2)
abline(lm(V$t2 ~ dobs) )


}
\keyword{misc}
