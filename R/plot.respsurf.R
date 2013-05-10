#' @S3method plot respsurf
plot.respsurf <-
function(x, ...){
  require(rgl)
  D1 <- x$data$D1
  D2 <- x$data$D2
  E <- x$data$E
  nxt10 <- ceiling(log10(length(E)))
  D1lim <- c(0, 1.1*max(D1))
  D2lim <- c(0, 1.1*max(D2))
  Elim <- c(0, 1.1*max(E))
  D1vec <- seq(from=0,to=D1lim[2],by=(D1lim[2]/(10^nxt10)))
  D2vec <- seq(from=0,to=D2lim[2],by=(D2lim[2]/(10^nxt10)))
  #?lapply
  plot3d(D1, D2 , E)
}
