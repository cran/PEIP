\name{setDesignG}
\alias{setDesignG}
\title{Set a Design Matrix.
}
\description{
Creata design matrix for simulating a
tomographic inversion on a simple grid.
}
\usage{setDesignG()}

\details{
Set up a simple design matrix
for tomographic in version.  This is used in examples
and illustrations of tomographics and matrix inversion methods. 
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\examples{
G = setDesignG()

### show the 56-th row
g = matrix( G[56,] , ncol=16, nrow=16)
imagesc(g)

\dontrun{
### show total coverage
zim = matrix(0 , ncol=16, nrow=16)
for(i in 1:dim(G)[1])
{
g = matrix( G[i,] , ncol=16, nrow=16)
zim =zim + g
}
image(zim)


}

}
\keyword{misc}
