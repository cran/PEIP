\name{quadlin}
\alias{quadlin}
\title{Lagrange multiplier technique
}
\description{Quadratic Linearization
}
\usage{
quadlin(Q, A, b)
}
\arguments{
  \item{Q}{positive definite symmetric matrix

}
  \item{A}{matrix with linearly independent rows

}
  \item{b}{data vector

}
}
\details{
  Solves the problem: min (1/2) t(x)*Q*x  with Ax = b.
  using the Lagrange multiplier technique, where Q is assumed to be
  symmetric and positive definite and the rows of A are linearly
 independent.

  
}
\value{list:
  \item{x}{vector of solution values}
  \item{lambda}{Lagrange multiplier}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

###%    Radius of the Earth (km)
    Re=6370.8;
rad = 5000
ri=rad/Re;

q=c(1.083221147,  1.757951474)
H = matrix(rep(0, 4), ncol=2, nrow=2)

H[1,1]=1.508616069 - 3.520104161*ri + 2.112062496*ri^2;
H[1,2]=3.173750352 - 7.140938293*ri + 4.080536168*ri^2;
H[2,1]=H[1,2];
H[2,2]=7.023621326 - 15.45196692*ri + 8.584426066*ri^2;
A1 =quadlin(H,t(q), 1.0 );


}

\keyword{misc}
