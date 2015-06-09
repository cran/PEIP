\name{l_curve_corner}
\alias{l_curve_corner}
\title{L Curve Corner
}
\description{Retrieve corner of L-curve
}
\usage{
l_curve_corner(rho, eta, reg_param)
}
\arguments{
  \item{rho}{misfit
}
  \item{eta}{model norm or seminorm

}
  \item{reg_param}{regularization parameter
}
}

\value{
    \item{reg_corner}{   the value of reg_param with maximum curvature}
    \item{ireg_corner}{  the index of the value in reg_param with maximum curvature}
    \item{kappa}{        the curvature for each reg_param}

}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
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



K1 =  l_curve_tgsvd(U1,t,X1,Lam1,G,L1);

rho1 =K1$rho
eta1 =K1$eta
reg_param1 =K1$reg_param
m1s =K1$m

### % store where the corner is (from visual inspection)
vcorn = l_curve_corner(rho1, eta1, reg_param1)

ireg_corner1=vcorn$reg_corner
rho_corner1=rho1[ireg_corner1];
eta_corner1=eta1[ireg_corner1];
print(paste('1st order reg corner is:  ',ireg_corner1));


plot(rho1,eta1,type="b", log="xy" , xlim=c(1e-4, 1e-2) , ylim=c(6e-6, 2e-4)  ,
     xlab="Residual Norm ||Gm-d||_2", ylab="Solution Seminorm ||Lm||_2"  );
points(rho_corner1, eta_corner1, col='red', cex=2 )


}
\keyword{misc}
