#' Normal Distribution Curve
#'
#' @param a is the start point to calculate the lower tail
#' @param mu is the mean of the distribution
#' @param sigma is the standard deviation of the distribution
#'
#' @return a graph of the distribution and the area of the lower tail probability, also gives you the lower tail probability
#' @export
#'
#' @examples myncurve = function(mu=10, sigma=2, p=9)
myncurve = function(mu, sigma, p){
  ### Draw a normal curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  # Find the area between x=-∞ and p
  prob=pnorm(p,mean=mu,sd=sigma)
  prob

  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(-9999,p,length=10000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(-9999,xcurve,p),c(0,ycurve,0),col="Red")

}
myncurve(mu=10, sigma=2, p=9)

