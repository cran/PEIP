\name{l_curve_tikh_svd}
\alias{l_curve_tikh_svd}
\title{L-curve Tikhonov
}
\description{L-curve for Tikhonov regularization
}
\usage{
l_curve_tikh_svd(U, s, d, npoints, varargin = NULL)
}
\arguments{
  \item{U}{matrix of data space basis vectors from the svd

}
  \item{s}{vector of singular values
}
  \item{d}{the data vector
}
  \item{npoints}{the number of logarithmically spaced regularization parameters
}
\item{varargin}{
  alpha_min, alpha_max: if specified, constrain the logrithmically spaced
   regularization parameter range, otherwise an attempt is made to estimate
   them from the range of singular values

}
}
\details{Calculates the L-curve
}
\value{
\item{eta}{the solution norm ||m|| or seminorm ||Lm||}
  \item{rho}{the residual norm ||G m - d||}
  \item{reg_param}{corresponding regularization parameters}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
####  Vertical Seismic Profile example
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

K1 = l_curve_tikh_svd(U1, diag(M1) , X1, 25, varargin = NULL)

rho1 =K1$rho
eta1 =K1$eta
reg_param1 =K1$reg_param
m1s =K1$m

###  store where the corner is (from visual inspection)
ireg_corner1=8;
rho_corner1=rho1[ireg_corner1];
eta_corner1=eta1[ireg_corner1];
print(paste("1st order reg corner is:  ",ireg_corner1));

plot(rho1,eta1,type="b", log="xy" ,
     xlab="Residual Norm ||Gm-d||_2", ylab="Solution Seminorm ||Lm||_2"  );

}

\keyword{misc}
