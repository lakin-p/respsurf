\name{rspsfFit}
\alias{rspsfFit}
\title{Fit a response surface to dose and endpoint data.}
\usage{
  rspsfFit(data = dwn.data, mpos = FALSE,
    start = list(B = 0.12, Econ = 1.19, IC50.1 = 1.5, IC50.2 = 3.5, m1 = -2.1, m2 = -2.2, alfa = 2),
    lower = list(B = 0, Econ = 0, IC50.1 = 1e-07, IC50.2 = 1e-07, m1 = -6, m2 = -6, alfa = 0.1),
    upper = list(B = 1, Econ = 5, EC50.1 = 20, EC50.2 = 20, m1 = -0.5, m2 = -0.5, alfa = 50),
    method = "greco95")
}
\arguments{
  \item{mpos}{A logical variable indicating whether the
  dose slopes are positive. Usually false. Defaults to
  false.}

  \item{start}{A list of numeric values, always of the form
  B, Econ, C50.1, C50.2, m1, m2, alpha. Defaults to a fair
  guess for the example data, dwn.data.}

  \item{lower}{A list of numeric values, always of the form
  B, Econ, C50.1, C50.2, m1, m2, alpha. Can and probably
  should be left null for final run. Defaults to fair
  guesses at lower bounds for the example data, dwn.data.}

  \item{upper}{A list of numeric values, always of the form
  B, Econ, C50.1, C50.2, m1, m2, alpha. Can and probably
  should be left null for final run. Defaults to fair
  guesses at upper bounds for the example data, dwn.data.}

  \item{data}{A matrix or data frame of points at which the
  response surface is to be constructed. Should have
  columns labeled 'D1', 'D2', and 'E'.}

  \item{method}{Method for conducting response surface
  analysis. Defaults to "greco95", only accepts this for
  now. Other methods may be added later.}
}
\value{
  An object of class 'respsurf'.
}
\description{
  Fit a response surface to dose and endpoint data.
}
\author{
  Paul Lakin
}

