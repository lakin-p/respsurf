#' @S3method summary respsurf
summary.respsurf <-
function(object, ...){
  para <- unlist(object$par)
  RSS <- object$deviance
  cat("Coefficients:\n")
  print(para)
  cat("Residual sum of squares:")
  print(RSS)
}
