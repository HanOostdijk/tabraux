% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cond_transpose.R
\name{cond_transpose}
\alias{cond_transpose}
\title{transpose above with octave when below certain note}
\usage{
cond_transpose(notes, lowest = "e,", numeric = F)
}
\arguments{
\item{notes}{character string with notes}

\item{lowest}{lowest note that will not be transposed}

\item{numeric}{boolean indicating (if TRUE that octaves are indicated with numbers "c" identical to "c3" )}
}
\value{
transposed piece of music
}
\description{
Useful for e.g. guitar where e (or d) is the lowest note
}
\examples{
\dontrun{
  cond_transpose("c, d, e, f,")
#  [1] "c d e, f,"
  cond_transpose("c, d, e, f,",lowest='d,')
#  [1] "c d, e, f,"
  cond_transpose("d4 a3 g#4 ^1 a3b3f4",lowest='d4',numeric=T)
# [1] "d4 a4 g#4 ^1 a4b4f4"
}
}
