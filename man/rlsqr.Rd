\name{rlsqr}
\alias{rlsqr}
\title{rlsqr
}
\description{R Version of LSQR routine written in Fortran by Paige and Saunders.
}
\usage{
rlsqr(G=matrix(), u=vector(), wantse = 0, damp = 0, atol = 0,
 btol = 0, conlim = 0, itnlim = 100, nout = 0)
}
\arguments{
  \item{G}{Design Matrix
}
  \item{u}{data vector
}
  \item{wantse}{logical, if standard estimates are desired
}
  \item{damp}{Damping parameter
}
  \item{atol}{a Tolorance
}
  \item{btol}{b Tolorance
}
  \item{conlim}{con
}
  \item{itnlim}{Iteration limit
}
  \item{nout}{integer, output file (Not Available)
}
}
\details{
  This code is an R wrapper for performing the LSQR
  routine by Paige and Saunders.  The LSQR program is a popular
  inversion program for solving the least squares problem, Ax=b.
  
  
}
\value{list:
\item{x}{solution vector}
\item{itn}{number of iterations}
\item{xnorm}{An estimate of the norm of the final solution vector x}
\item{rnorm}{norm(rbar), function being minimized} 
\item{arnorm}{norm( Abar(transpose)*rbar),
           norm of the residual for the usual normal equations}
\item{acond}{An estimate of cond(Abar)} 
\item{anorm}{An estimate of the Frobenius norm of  Abar}
\item{istop}{reason for stopping:0=exact solution, 1=okay, 2=damp is zero,
      3=damp is nonzero, 4=condition problem, 5=iteration limit reached}
}
\references{
  Paige, C., and Saunders, M.,
  LSQR: An algorithm for sparse linear equations and sparse least
  squares.
  Trans. Math. Software, 1982, 8, 43-71
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>, Keehoon Kim<keehoon@email.unc.edu>
}
\note{
  The fortran code has write statements
  that are not available in R-code according to the
  standard R documentation.
}


\seealso{sirt, art
}
\examples{

\dontrun{


library(png)
library(PEIP)
shift=function(x,ns)
{
nsig=c(rep(0,ns),x[1:(length(x)-ns)])
return(nsig)
}

imagesc<-function(G, col=grey((1:99)/100), ... )
  {
    #########  plot an image after flipping and transposing
    ###   to match the matlab code
   d = dim(G)
   b = G[d[1]:1,]
   image(t(b), col=col, ...)
  }
img= readPNG('original_image.png')
im = matrix(as.vector(img) , 40000, 1)

load('rtest.RDATA')


#################################################
# Blurring the image (DATA == d)
#################################################

d0=G \%*\% im
d=d0


#################################################
# Solving damped least squares problem
# (    G     ) * x  =  ( d )
# ( damp * I )         ( 0 )
#################################################

# damp = lambda
lambda=0

# data vector (u)
u=d

# tolerance of iteration
atol=1.0e-6;btol=1.0e-6
#atol=.Machine$double.eps;btol=.Machine$double.eps

# taking condition number of G into consideration ( 0 means to ignore it )
conlim=0

# the number of iteration of LSQR
itnlim=1000;



#################################################
# Running LSQR
#################################################

# Executing LSQR
lx=rlsqr(G=G,u=u,damp=lambda,itnlim=itnlim,atol=atol,btol=btol,conlim=0)
# lx$x : model vector inverted from LSQR
# lx$xnorm : || x ||^2
# lx$rnorm : || Gx - d ||^2 (Norm of residual)
# lx$itn : the number of iteration to get the result


# Results of LSQR


layout(matrix(1:3, ncol=3))
imagesc(matrix(img,200,200),main='Original Image',axes=FALSE, xlab='',ylab='')
imagesc(matrix(d,200,200),main='Blurred Image',axes=FALSE,xlab='',ylab='')
imagesc(matrix(lx$x,200,200),
main=paste('LSQR solution Itr. = ',lx$itn),axes=FALSE,xlab='',ylab='')
}



}
\keyword{misc}
