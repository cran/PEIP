\name{error.bar}
\alias{error.bar}
\title{Plot Error Bar
}
\description{Plot Error Bar
}
\usage{
error.bar(x, y, lo, hi, pch = 1, col = 1, barw = 0.1, add = FALSE, ...)
}

\arguments{
  \item{x}{X-values
}
  \item{y}{Y-values
}
  \item{lo}{Lower limit of error bars
}
  \item{hi}{Upper limit of error bars
}
  \item{pch}{plotting character
}
  \item{col}{color
}
  \item{barw}{width of the bar
}
  \item{add}{logical, add=FALSE starts a new plot
}
  \item{\dots}{other plotting parameters
}
}

\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
x = 1:10
y = 2*x+5
zup = rnorm(10)


zup = zup-min(zup)+.5
zdown = rnorm(10)
zdown = zdown-min(zdown)+.2


####  example with same error on either side:
error.bar(x, y, y-zup, y+zup, pch = 1, col = 'brown' , barw = 0.1, add =
FALSE)


####  example with different  error on either side:
error.bar(x, y, y-zdown, y+zup, pch = 1, col = 'brown' , barw = 0.1, add
= FALSE)




}
\keyword{misc}
