\name{gr95Model}
\alias{gr95Model}
\title{A vectorized root finder to model a Greco 1995 surface
for two vectors of concentrations.}
\usage{
  gr95Model(params, D1, D2, mup)
}
\arguments{
  \item{mup}{A logical variable indicating whether the dose
  slopes are positive. Usually false.}

  \item{params}{A numeric vector, always of the form B,
  Econ, C50.1, C50.2, m1, m2, alpha.}

  \item{D1}{A vector of concentrations of substance 1 at
  which the response surface is to be constructed.}

  \item{D2}{A vector of concentrations of substance 2 at
  which the response surface is to be constructed.}
}
\description{
  A vectorized root finder to model a Greco 1995 surface
  for two vectors of concentrations.
}
\author{
  Paul Lakin
}

