\name{picard_vals}
\alias{picard_vals}
\title{Picard plot
}
\description{
  Picard plot parameters for subsequent plotting
}
\usage{
picard_vals(U, sm, d)
}
\arguments{
  \item{U}{the U matrix from the SVD or GSVD

}
  \item{sm}{singular values in decreasing order, or the lambdas divided by the mus
        in decreasing order

}
  \item{d}{data to fit

}
}

\value{List:
  \item{utd}{the columns of U transposed times d}
  \item{utd_norm}{utd./sm}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\keyword{misc}
