#' A vectorized root finder to model a Greco 1995 surface
#' for two vectors of concentrations.
#' 
#' @param mup A logical variable indicating whether the
#' dose slopes are positive. Usually false.
#' @param params A numeric vector, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha.
#' @param data A 2-column matrix of points at which the 
#' response surface is to be constructed.
#' 
#' @aliases gr95Model
#' @author Paul Lakin
#' 

gr95Model <-
function(params, data, mup){
  if(mup){
    L <- rep(params[1], times = dim(data)[1])
    U <- rep(params[2], times = dim(data)[1])
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (data[, 1] / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (data[, 2] / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * data[, 1] * data[, 2]) / (params[3] * params[4] * 
           (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      L[G >= 0] <- M[G >= 0]
      U[G < 0] <- M[G < 0]
    }
    E <- M
    E[data[, 1] == 0 && data[, 2] == 0] <- params[1]
    E
  }
  else{
    L <- rep(params[1], times = dim(data)[1])
    U <- rep(params[2], times = dim(data)[1])
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (data[, 1] / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (data[, 2] / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * data[, 1] * data[, 2]) / (params[3] * params[4] * 
         (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      L[G <= 0] <- M[G <= 0]
      U[G > 0] <- M[G > 0]
    }
    E <- M
    E[data[, 1] == 0 && data[, 2] == 0] <- params[2]
    E
  }
}
