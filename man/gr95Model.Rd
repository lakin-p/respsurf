\name{gr95Model}
\alias{gr95Model}
\title{A vectorized root finder to model a Greco 1995 surface
for two vectors of concentrations.}
\usage{
  gr95Model(params, data, mup)
}
\arguments{
  \item{mup}{A logical variable indicating whether the dose
  slopes are positive. Usually false.}

  \item{params}{A numeric vector, always of the form B,
  Econ, C50.1, C50.2, m1, m2, alpha.}

  \item{data}{A 2-column matrix of points at which the
  response surface is to be constructed.}
}
\description{
  A vectorized root finder to model a Greco 1995 surface
  for two vectors of concentrations.
}
\author{
  Paul Lakin
}

