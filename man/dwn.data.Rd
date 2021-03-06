\name{dwn.data}
\alias{dwn.data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Downsim data file. Default data file.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This datafile is simulated; it is not from a real laboratory experiment. However, it is based upon the actual data and modeling of real data from a published experiment reported in Figure 2, pp 3039 from Faessel et al (1998) [Faessel HM, Slocum HK, Jackson RC, Boritzki TJ, Rustum YM, Nair MG and Greco WR: Super in vitro synergy between inhibitors of dihydrofolate reductase and inhibitors of other folate-requiring enzymes: The critical role of polyglutamylation. Cancer Res 58:3036-3050, 1998].
}
\usage{data(dwn.data)}
\format{
  A data frame with 871 observations on the following 5 variables.
  \describe{
    \item{\code{G}}{a group variable}
    \item{\code{E}}{a vector of endpoint data}
    \item{\code{D1}}{a vector of concentration data}
    \item{\code{D2}}{a vector of concentration data}
    \item{\code{LC}}{a group variable for level curves}
  }
}
\examples{
data(dwn.data)
}
\keyword{datasets}
