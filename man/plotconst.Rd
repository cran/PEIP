\name{plotconst}
\alias{plotconst}
\title{Plot constant model
}
\description{
 Plots a model in piecewise constant form over n subintervals,
 where n is the length of x.
 
}
\usage{
plotconst(x, l, r, ...)
}
\arguments{
  \item{x}{model to be plotted

}
  \item{l}{left endpoint of plot

}
  \item{r}{right endpoint of plot

}
  \item{\dots}{graphical parameters 
}
}
\details{Used for plotting vector models
}
\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{linesconst
}

\examples{

zip = runif(25)
plotconst(zip, 0, 1 )
linesconst(runif(25) , 0, 1 , col='red' )



}

\keyword{misc}
