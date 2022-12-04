#' @title myci function (Confidence interval for the mean of a single sample)
#'
#' @param x sample
#' @param ci  confidence level of the interval
#' @param ...
#'
#' @return a range of the desired confidence interval for the mean of a single sample
#' @export
#'
#' @examples
#' myci(c(3,4,5,6))
myci=function(x,ci=0.95){

  n=length(x)
  con=1-((1-ci)/2)
  t=qt(con,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci

}


