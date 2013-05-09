\name{gr95Resid}
\alias{gr95Resid}
\title{Returns the residuals of the Greco 1995 model for a given
set of parameters versus concentration data and observed
endpoint data.}
\usage{
  gr95Resid(param, dlist, evec, mpos)
}
\arguments{
  \item{mpos}{A logical variable indicating whether the
  dose slopes are positive. Usually false.}

  \item{param}{A list of numeric values, always of the form
  B, Econ, C50.1, C50.2, m1, m2, alpha.}

  \item{dlist}{A 2-column matrix of points at which the
  response surface is to be constructed.}

  \item{evec}{A vector of observed endpoint values. Should
  be the same length as nrow(dlist).}
}
\description{
  Returns the residuals of the Greco 1995 model for a given
  set of parameters versus concentration data and observed
  endpoint data.
}
\author{
  Paul Lakin
}

