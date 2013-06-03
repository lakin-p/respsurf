#' @S3method plot respsurf
plot.respsurf <-
function(x, ...){
  require(rgl)
  D1 <- x$data$D1 ## our D1 data
  D2 <- x$data$D2 ## our D2 data
  E <- x$data$E   ## our E data
  nxt10 <- ceiling(log10(length(E))) ## poss. how many points we want to plot?
  D1lim <- c(0, 1.1*max(D1)) ## upper limit on dose 1 plot
  D2lim <- c(0, 1.1*max(D2)) ## upper limit on dose 2 plot.
  Elim <- c(0, 1.1*max(E))   ## upper limit on E plot.
  # deprecated:
  # D1vec <- seq(from=0,to=D1lim[2],by=(D1lim[2]/(10^nxt10)))
  # D2vec <- seq(from=0,to=D2lim[2],by=(D2lim[2]/(10^nxt10)))
  xvals <- seq(0, D1lim[2], length = 100)
  yvals <- seq(0, D2lim[2], length = 100)
  f <- function(a, b, par, mpos){
    gr95Model(params = par, D1 = a, D2 = b, mup = mpos)
  }
  z <- outer(X = xvals, 
             Y = yvals, 
             FUN = f, 
             par = as.numeric(x$par), 
             mpos = x$mpos)
  open3d(windowRect = c(30, 30, 600, 600))
  bg3d("white")
  plot3d(D1, D2 , E)
  open3d(windowRect = c(30, 30, 600, 600))
  plot3d(D1, D2 , x$fitted)
}
