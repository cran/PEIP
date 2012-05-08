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

\keyword{misc}
