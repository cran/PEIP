\name{l_curve_tgsvd}
\alias{l_curve_tgsvd}
\title{L curve tgsvd
}
\description{
  L curve parematers and models for truncated gsvd regularization.
}
\usage{
l_curve_tgsvd(U, d, X, Lam, G, L)
}
\arguments{
  \item{U}{U, output of GSVD
}
  \item{d}{ output of GSVD
}
  \item{X}{output of GSVD
}
  \item{Lam}{output of GSVD
}
  \item{G}{output of GSVD
}
  \item{L}{output of GSVD
}
}

\value{
List:
  \item{eta}{        the solution seminorm ||Lm||}
  \item{rho }{       the residual norm ||G m - d||}
  \item{reg_param}{  corresponding regularization parameters}
  \item{m}{          corresponding suite of models for truncated GSVD}


  
}
\references{ Aster, R.C., C.H. Thurber, and B. Borchers,
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
ireg_corner1=8;
rho_corner1=rho1[ireg_corner1];
eta_corner1=eta1[ireg_corner1];
print(paste('1st order reg corner is:  ',ireg_corner1));


plot(rho1,eta1,type="b", log="xy", xlim=c(1e-4, 1e-2) , ylim=c(6e-6, 2e-4) ,
     xlab="Residual Norm ||Gm-d||_2", ylab="Solution Seminorm ||Lm||_2"  );




}



\keyword{misc}
