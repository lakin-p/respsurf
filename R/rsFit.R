#' Fit a response surface to dose and endpoint data.
#' 
#' @param mpos A logical variable indicating whether the
#' dose slopes are positive. Usually false. Defaults to false.
#' @param start A list of numeric values, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha. Defaults to a fair guess
#' for the example data, dwn.data.
#' @param lower A vector of numeric values, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha. Can and probably should
#' be left null for final run. Defaults to fair guesses at lower
#' bounds for the example data, dwn.data.
#' @param upper A vector of numeric values, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha. Can and probably should
#' be left null for final run. Defaults to fair guesses at upper
#' bounds for the example data, dwn.data.
#' @param data A matrix or data frame of points at which the 
#' response surface is to be constructed. Should have columns
#' labeled 'D1', 'D2', and 'E'.
#' @param method Method for conducting response surface analysis.
#' Defaults to "greco95", only accepts this for now. Other methods
#' may be added later.
#' 
#' @return An object of class 'respsurf'.
#' @examples
#' data(dwn.data)
#' demofit.dn <- rsFit(data = dwn.data)  
#' ## fit a response surface to the dwn.data data
#' summary(demofit.dn)
#' plot(demofit.dn)
#' 
#' data(ups.data)
#' demofit.up <- rsFit(data  = ups.data,
#'                        start = list(B = 0.12, Econ = 1.19, EC50.1 = 1.4, 
#'                                     EC50.2 = 3.4, m1 = 2.0, m2 = 2.1, 
#'                                     alfa = 2.0),
#'                        mpos = TRUE
#' )
#' summary(demofit.up)
#' plot(demofit.up)
#' 
#' @aliases rsFit
#' @author Paul Lakin
#' 


rsFit <- function(
  data = NULL, ## user should give a data frame containing names D1, D2,
                   #  and E. a matrix will also do. 
  mpos = FALSE, ## in general we are looking at the case where we slope down
  start = list( ## in general user should give their own start parameters
    B = 0.12, 
    Econ = 1.19, 
    IC50.1 = 1.5, 
    IC50.2 = 3.5,
    m1 = -2.1, 
    m2 = -2.2, 
    alfa = 2.0
  ),
#   lower = c(B = 0, ## users should specify null if they want unlimited search
#                Econ = 0, 
#                IC50.1 = .0000001, 
#                IC50.2 = .0000001,
#                m1 = -10, 
#                m2 = -10, 
#                alfa = -50
#   ),
  lower = NULL,
#   upper = c(B = 1, ## users should specify null if they want unlimited search
#                Econ = 5, 
#                IC50.1 = 20, 
#                IC50.2 = 20,
#                m1 = -0.01, 
#                m2 = -0.01, 
#                alfa = 50
#   ),
  upper = NULL,
  method = "greco95" ## for now this is the only option
)
{
  require(minpack.lm)
  out <- nls.lm(par = start,
                lower = lower,
                upper = upper,
                fn = gr95Resid,
                control = nls.lm.control(maxiter=1024),
                dlist = data[, c("D1", "D2")],
                evec = data[, "E"],
                mpos = mpos
                )
  out$data <- list(D1 = NULL)
  out$data$D1 <- data[, "D1"]
  out$data$D2 <- data[, "D2"]
  out$data$E <- data[, "E"]
  out$mpos <- mpos
  out$fitted <- gr95Model(params = as.numeric(out$par),
                          D1 = out$data$D1,
                          D2 = out$data$D2,
                          mup = mpos)
  class(out) <- "respsurf"
  out
}
