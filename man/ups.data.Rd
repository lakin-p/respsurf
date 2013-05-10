\name{ups.data}
\alias{ups.data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Upsim dataset.
}
\description{
This datafile is simulated; it is not from a real laboratory experiment. However, it is based upon the actual data and modeling of real data from a published experiment reported in Figure 2, pp 3039 from Faessel et al (1998) [Faessel HM, Slocum HK, Jackson RC, Boritzki TJ, Rustum YM, Nair MG and Greco WR: Super in vitro synergy between inhibitors of dihydrofolate reductase and inhibitors of other folate-requiring enzymes: The critical role of polyglutamylation. Cancer Res 58:3036-3050, 1998].
}
\usage{data(ups.data)}
\format{
  A data frame with 871 observations on the following 3 variables.
  \describe{
    \item{\code{E}}{a vector of endpoint data}
    \item{\code{D1}}{a vector of concentration data}
    \item{\code{D2}}{a vector of concentration data}
  }
}
\examples{
data(ups.data)
## maybe str(ups.data) ; plot(ups.data) ...
}
\keyword{datasets}
