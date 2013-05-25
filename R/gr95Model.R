#' A vectorized root finder to model a Greco 1995 surface
#' for two vectors of concentrations.
#' 
#' @param mup A logical variable indicating whether the
#' dose slopes are positive. Usually false.
#' @param params A numeric vector, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha.
#' @param D1 A vector of concentrations of substance 1 at which the 
#' response surface is to be constructed.
#' @param D2 A vector of concentrations of substance 2 at which the 
#' response surface is to be constructed.
#' 
#' @aliases gr95Model
#' @author Paul Lakin
#' 

gr95Model <-
function(params, D1, D2, mup){
  if(mup){
    L <- rep(params[1], times = length(D1))
    U <- rep(params[2], times = length(D1))
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (D1 / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (D2 / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * D1 * D2) / (params[3] * params[4] * 
           (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      L[G >= 0] <- M[G >= 0]
      U[G < 0] <- M[G < 0]
    }
    E <- M
    E[D1 == 0 && D2 == 0] <- params[1]
    E
  }
  else{
    L <- rep(params[1], times = length(D1))
    U <- rep(params[2], times = length(D1))
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (D1 / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (D2 / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * D1 * D2) / (params[3] * params[4] * 
         (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      L[G <= 0] <- M[G <= 0]
      U[G > 0] <- M[G > 0]
    }
    E <- M
    E[D1 == 0 && D2 == 0] <- params[2]
    E
  }
}
