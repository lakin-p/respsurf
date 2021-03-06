#' @name respsurf-package
#' @aliases respsurf
#' @docType package
#' @title Tools for fitting a response surface to dosage data
#' @author Paul Lakin
#' \tabular{ll}{
#' Version: \tab 0.1.3 \cr
#' Date: \tab 2013-05-09 \cr
#' Maintainer: \tab Paul Lakin \email{prl26@@case.edu} \cr
#' }
#' @description respsurf fits response surfaces to dosage data.
#' It currently only supports the model of Greco et al. of 1995,
#' but may be expanded to include other response surface models,
#' and even other assessments of interactions by dosage, such
#' as the method of Chou-Talalay.
#' @examples
#' data(dwn.data)
#' demofit.dn <- rsFit(data = dwn.data)  
#' ## fit a response surface to the dwn.data data
#' summary(demofit.dn)
#' plot(demofit.dn)
#' 
#' data(ups.data)
#' demofit.up <- rsFit(data  = ups.data,
#'                     start = list(B = 0.12, Econ = 1.19, EC50.1 = 1.4,
#'                                  EC50.2 = 3.4, m1 = 2.0, m2 = 2.1,
#'                                  alfa = 2.0),
#'                     lower = list(B = 0, Econ = 0, EC50.1 = .0000001, EC50.2 = .0000001,
#'                                  m1 = .1, m2 = .1, alfa = .1),
#'                     upper = list(B = 1, Econ = 5, EC50.1 = 20, EC50.2 = 20,
#'                                  m1 = 6, m2 = 6, alfa = 50),
#'                     mpos = TRUE
#' )
#' summary(demofit.up)
#' plot(demofit.up)
#' @import rgl minpack.lm

NULL

