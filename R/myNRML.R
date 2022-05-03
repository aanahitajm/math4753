#' @title Newton Rhapson maximum likelihood
#'
#' @param x0 the initial x point at which to start analysing the graph to see where it intersects x-axis
#' @param delta the difference between x points analysed
#' @param llik function you want to implement. Must be a log base e function
#' @param xrange the range of the x axis for the log likelihood graph
#' @param parameter the parameter that is to be estimate eg. mu, sigma, lambda
#' @importFrom graphics axis layout
#' @return returns two graphs, first the log likelihood graph of the parameter of interest. Second the derivative of the log likelihood graph that shows the different x points analysed and the final x point at which the graph intersects the x axis
#' @export
#'
#' @examples
#'   \dontrun{myNRML(x0=1,delta=0.000001,llik=function(x) log(dpois(4,x)*dpois(5,x)),xrange=c(0,20),parameter="lambda")}
myNRML=function(x0,delta=0.001,llik,xrange,parameter="param"){
  f=function(x) (llik(x+delta)-llik(x))/delta
  fdash=function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nrow=1,ncol=2,byrow=TRUE),widths=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x,y=y)
}

globalVariables("points")
