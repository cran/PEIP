\name{shawG}
\alias{shawG}
\title{Shaw Model of Slit Diffraction
}
\description{
Creates the design matrix for the Shaw inverse problem
of diffraction through a narrow slot.
}
\usage{
shawG(m, n)
}
\arguments{
  \item{m}{integer, number of rows
}
  \item{n}{integern number of columns
}
}
\details{
See Aster's book for a details explaination.
}
\value{
Matrix used for creating data and inversion.
}
\references{
C. B. Shaw, Jr., "Improvements of the resolution of 
an instrument by numerical solution of an integral equation", 
J. Math. Anal. Appl. 37: 83-112, 1972. 
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
n = 20
G = shawG(n,n)

spike = rep(0,n)
spike[10] = 1

dspiken = G \%*\%  spike

plot(dspiken)



}
\keyword{misc}
