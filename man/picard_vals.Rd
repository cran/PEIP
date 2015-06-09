\name{picard_vals}
\alias{picard_vals}
\title{Picard plot
}
\description{
  Picard plot parameters for subsequent plotting.
}
\usage{
picard_vals(U, sm, d)
}
\arguments{
  \item{U}{the U matrix from the SVD or GSVD

}
  \item{sm}{singular values in decreasing order, or the GSVD lambdas divided by the mus
        in decreasing order
}
  \item{d}{data to fit, right hand side

}
}

\value{List:
  \item{utd}{the columns of U transposed times d}
  \item{utd_norm}{utd./sm}

}
\details{ The Picard plot is a method of helping to determine
regularization schemes.}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\seealso{GSVD}

\examples{
####
n = 20
G = shawG(n,n)
spike = rep(0,n)
spike[10] = 1
dspiken = G %*%  spike

set.seed(2015)
dspiken = dspiken + 6e-6 *rnorm(length(dspiken))
Utube=svd(G);
U = Utube$u
V = Utube$v
S = Utube$d
s=Utube$d
 R3 = picard_vals(U,s,dspiken);
utd = R3$utd
utd_norm= R3$utd_norm
###  Produce the Picard plot.

x_ind=1:length(s);
##  
plot( range(x_ind) , range(c(s ,abs(utd),abs(utd_norm))),
          type='n',  log='y', xlab="i", ylab="" )
lines(x_ind,s, col='black')
points(x_ind,abs(utd), pch=1, col='red')
points(x_ind,abs(utd_norm), pch=2, col='blue')

title("Picard Plot for Shaw Problem")




}

\keyword{misc}
