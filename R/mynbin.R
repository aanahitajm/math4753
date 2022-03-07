#' @title mynbin
#'
#' @param y y is the number of trials until the rth success is observed
#' @param r the number of successes
#' @param p the probability of success on a single bernoulli tria;
#'
#' @return Returns the negative Binomial probability of an event, i.e., the number of failures until the rth success
#' @export
#'
#' @examples
#'    \dontrun{mynbin(10,3,0.4)}

mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

