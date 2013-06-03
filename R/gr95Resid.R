#' Returns the residuals of the Greco 1995 model for a given
#' set of parameters versus concentration data and observed
#' endpoint data.
#' 
#' @param mpos A logical variable indicating whether the
#' dose slopes are positive. Usually false.
#' @param param A list of numeric values, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha.
#' @param dlist A 2-column matrix of points at which the 
#' response surface is to be constructed.
#' @param evec A vector of observed endpoint values. Should be
#' the same length as nrow(dlist).
#' 
#' @aliases gr95Resid
#' @author Paul Lakin
#' 


gr95Resid <-
function(param, dlist, evec, mpos){
  # Take a vector of parameters of the greco1995 model
  # and spit out a vector of the residuals data.E - model.E
  
  evec - gr95Model(params = as.numeric(param), 
                   D1 = dlist[, 1], 
                   D2 = dlist[, 2],
                   mup = mpos)
}
