\name{art}
\alias{art}
\title{ART Inverse solution
}
\description{ART algorythm for solving
  sparse linear inverse problems
}
\usage{
art(A, b, tolx, maxiter)
}
\arguments{
  \item{A}{Constraint matrix
}
  \item{b}{right hand side
}
  \item{tolx}{difference tolerance for successive iterations (stopping criteria)
}
  \item{maxiter}{ maximum iterations (stopping criteria).
}
}
\details{
  Alpha is a damping factor.  If alpha<1, then we won't take full steps
 in the ART direction.  Using a smaller value of alpha (say alpha=.75)
 can help with convergence on some problems.

}
\value{
  \item{x}{solution}
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


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

### % Compute the data.
dtrue=G \%*\% mtruev;

### % Add the noise.


d=dtrue+0.01*rnorm(length(dtrue));

mkac<-art(G,d,0.01,200)
par(mfrow=c(1,2))
imagesc(matrix(mtruem,16,16) , asp=1 , main="True Model" );

imagesc(matrix(mkac,16,16) , asp=1 , main="ART Solution" );


}

\keyword{misc}
