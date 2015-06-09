\name{gcval}
\alias{gcval}
\title{Get c-val
}
\description{ Extract the smallest regularization parameter.

}
\usage{
gcval(U, s, b, npoints)
}
\arguments{
  \item{U}{U matrix from gsvd(G, L)

}
  \item{s}{[diag(C) diag(S)] which are the lambdas and mus from the gsvd

}
  \item{b}{the data to try and match

}
  \item{npoints}{number of alphas to estimate

}
}
\details{Evaluate the GCV function gcv_function at npoints points.

}
\value{List:
 \item{reg_min}{ alpha with the minimal g (scalar)}
 \item{g }{ || Gm_(alpha,L) - d ||^2 / (Tr(I - GG#)^2}
 \item{alpha}{alpha for the corresponding g}

  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{gcv_function
}

\examples{

set.seed(2015)
VSP = vspprofile()
t = VSP$t2
G = VSP$G
M = VSP$M
N = VSP$N

L1 = get_l_rough(N,1);
littleU = PEIP::GSVD(as.matrix(G), as.matrix(L1) );

BIGU = flipGSVD(littleU, dim(G), dim(L1) )

U1 = BIGU$U
V1 =BIGU$V
X1=BIGU$X
Lam1=BIGU$C
M1=BIGU$S

lam=sqrt(diag(t(Lam1 \%*\% Lam1)));

mu=sqrt(diag(t(M1)\%*\%M1));

p=rnk(L1);

sm1=cbind(lam[1:p],mu[1:p])

### % get the gcv values varying alpha

###
ngcvpoints=1000;

HI = gcval(U1,sm1,t,ngcvpoints);



}

\keyword{misc}
