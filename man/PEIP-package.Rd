\name{PEIP-package}
\alias{PEIP-package}
\alias{PEIP}
\docType{package}
\title{
 Inverse Theory Functions for PEIP book
}
\description{
 Auxilliary functions and routines for running the
  examples and excersizes described in the book
  on inverse theory.
}
\details{
\tabular{ll}{
Package: \tab PEIP\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2012-05-08\cr
License: \tab GPL\cr
}

These functions are used in conjunction with the example described in
the PEIP book.

There is one C-code routine, interp2grid.
This is introduced to replicate the MATLAB code interp2.
It does not work exactly as the matlab code prescribes.

In the PEIP library one LAPACK routine
is called: dggsvd.
In R, LAPACK routines are stored in slightly
different locations on Linux, Windows and Mac computers.
Be aware. This will come up in examples from Chapter 4.

Almost all examples work as scripts 
 run with virtually no user input, e.g.


}
\author{
Jonathan M. Lees<jonathan.lees.edu>
Maintainer:Jonathan M. Lees<jonathan.lees.edu>
}
\references{Aster, R.C., C.H. Thurber, and B. Borchers,
  \emph{Parameter Estimation and Inverse Problems}, Elsevier Academic Press, Amsterdam, 2005.
}

\keyword{ package }
