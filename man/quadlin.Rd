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


\keyword{misc}
