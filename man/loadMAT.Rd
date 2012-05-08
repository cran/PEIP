\name{loadMAT}
\alias{loadMAT}
\title{Load a Matlab matfile
}
\description{Load a Matlab matfile, rename the internal parameters
  to get R-objects
}
\usage{
loadMAT(fn, pos=1)
}
\arguments{
  \item{fn}{file name of MATfile
  }
  \item{pos}{ integer, position in search path, default=1 }
}
\details{Program reads in previously saved mat-files and extracts the
  data, and renames the variables to match the book.
}
\value{Whatever is in the MATfile
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Matfiles are created using the matlab2R routines
}



\keyword{misc}
