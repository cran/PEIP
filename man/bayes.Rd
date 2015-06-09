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
\examples{

\dontrun{
set.seed(2015)
G = setDesignG()
### % Setup the true model.
mtruem=matrix(rep(0, 16*16), ncol=16,nrow=16);

mtruem[9,9]=1; mtruem[9,10]=1; mtruem[9,11]=1;
mtruem[10,9]=1; mtruem[10,11]=1;
mtruem[11,9]=1; mtruem[11,10]=1; mtruem[11,11]=1;
mtruem[2,3]=1; mtruem[2,4]=1;
mtruem[3,3]=1; mtruem[3,4]=1;

### % reshape the true model to be a vector
mtruev=as.vector(mtruem);
imagesc(matrix(mtruem,16,16) , asp=1 , main="True Model" );


matrix(mtruem,16,16) , asp=1 , main="True Model" )


### % Compute the data.
dtrue=G \%*\% mtruev;

### % Add the noise.
d=dtrue+0.01*rnorm(length(dtrue));
covd = 0.1*diag( nrow=length(d) )
covm = 1*diag( nrow=dim(G)[2] )
}
}



\keyword{misc}
