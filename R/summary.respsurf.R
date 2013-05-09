#' @title summary.respsurf

summary.respsurf <-
function(rsp){
  para <- unlist(rsp$par)
  RSS <- rsp$deviance
  cat("Coefficients:\n")
  print(para)
  cat("Residual sum of squares:")
  print(RSS)
}
