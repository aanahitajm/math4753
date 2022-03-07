#' @title myncurve
#'
#' @param mu #the mean of the data
#' @param sigma #standard deviation of the data
#' @param a #the value of x up to where the area under the graph should be colored
#' @importFrom graphics text curve polygon
#' @importFrom stats dnorm pnorm
#' @return returns a plot of the normal distribution with the area/probability shaded in
#'         and written on the graph. Also lists the area into the command-line
#'
#' @export
#'
#' @examples
#'    \dontrun{myncurve(10, 5, 6)}
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col ="red", ylab = "Normal Density")
  xcurve=seq(-1000,a,length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(-1000,xcurve,a),c(0,ycurve,0),col="blue")
  prob=pnorm(a,mu,sigma)
  prob=round(prob,4)

  text(mu+2*sigma,1, paste("Area = ", prob, sep=""))
  list(Area = prob)
}

globalVariables("x")

