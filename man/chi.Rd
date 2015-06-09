\name{chi}
\alias{chi}
\title{Chi function
}
\description{Chi function
}
\usage{
chi(x, n)
}
\arguments{
  \item{x}{value
}
  \item{n}{degrees of freedom
}
}

\value{function evaluated
}
\references{ Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

x = seq(0, 10, length=100)
n = 5
y=chi(x, n)
plot(x, y)


}

\keyword{misc}
