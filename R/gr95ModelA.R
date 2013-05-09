#' An atomic root finder to model a Greco 1995 surface
#' for two vectors of concentrations. Not currently used:
#' planned for surface plotting.
#' 
#' @param mup A logical variable indicating whether the
#' dose slopes are positive. Usually false.
#' @param params A numeric vector, always of the form
#' B, Econ, C50.1, C50.2, m1, m2, alpha.
#' @param data A 2-entry vector, a point at which the 
#' response is to be modelled.
#' 
#' @aliases gr95ModelA
#' @author Paul Lakin
#' 

gr95ModelA <-
function(params, data, mup){
  if(mup){
    L <- params[1]
    U <- params[2]
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (data[1] / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (data[2] / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * data[, 1] * data[, 2]) / (params[3] * params[4] * 
                                                   (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      if (G >= 0) L<- M
       else U <- M
    }
    E <- M
    if(data[1] == 0 & data[2] == 0) E <- params[1]
    E
  }
  else{
    L <- params[1]
    U <- params[2]
    for (i in 1:50){
      ## 50 iterations
      M <- (L + U) /2
      G <- (data[1] / (params[3] * (M / (params[2] - M)) ^ (1 / params[5]))) +
        (data[2] / (params[4] * (M / (params[2] - M)) ^ (1 / params[6]))) +
        (
          (params[7] * data[, 1] * data[, 2]) / (params[3] * params[4] * 
                                                   (M / (params[2] - M)) ^ (.5/params[5] + .5/params[6]))
        ) - 1
      if (G <= 0) L <- M
      else U <- M
    }
    E <- M
    if (data[1] == 0 & data[2] == 0) E <- params[2]
    E
  }
}
