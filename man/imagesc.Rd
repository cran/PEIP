\name{imagesc}
\alias{imagesc}
\alias{contoursc}

\title{Image Display
}
\description{Display image in matlab format, i.e. flip and transpose.
}
\usage{
imagesc(G, col = grey((1:99)/100), ...)
contoursc(G, ...) 
}
\arguments{
  \item{G}{Image matrix
}
  \item{col}{color scale
}
  \item{\dots}{graphical parameters
}
}
\details{
  Program flips image and transposes prior to plotting.
  The contour version does the same and can be used to add contours.
}
\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\examples{
mtruem=matrix(rep(0, 16*16), ncol=16,nrow=16);

mtruem[9,9]=1; mtruem[9,10]=1; mtruem[9,11]=1;
mtruem[10,9]=1; mtruem[10,11]=1;
mtruem[11,9]=1; mtruem[11,10]=1; mtruem[11,11]=1;
mtruem[2,3]=1; mtruem[2,4]=1;
mtruem[3,3]=1; mtruem[3,4]=1;

imagesc(mtruem, asp=1)



}
\keyword{misc}
