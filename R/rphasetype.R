#' @title Markov Chain Monte Carlo phase type parameters estimation
#' @description Greet a person and appropriately capitalize their name.
#'
#' @param n Your name (character string; e.g. "john doe").
#' @param alpha Your name (character string; e.g. "john doe").
#' @param T Your name (character string; e.g. "john doe").
#'
#' @return A Markov Chain Monte Carlo
#'
#' @export
#'
#' @examples
#'library(bnpPhaseType)
#'
#' @importFrom stats rmultinom rexp rbinom
rPhDistribution <- function(n, alpha=alpha, T=T)
{
  # uniformization
  q <- length(alpha)
  lamb <- max(-diag(T))
  P = (1/lamb)*T + diag(q)
  replicate(n, rph(alpha=alpha, lamb = lamb, P=P, t=0))
}

rph <- function(alpha, lamb, P, t)
  {
  q <- length(alpha)
  k <- (1:q)[rmultinom(1, size = 1, prob = alpha) == 1]
  
  if(t == 0){
    rph(alpha = P[k,], lamb=lamb, P=P, t=rexp(n=1, rate = lamb))
  }
  else{
    p <- sum(P[k,])
    if(rbinom(n=1, size=1, prob = p) == 1){
      rph(alpha = P[k,], lamb=lamb, P=P, t=t + rexp(n=1, rate = lamb))
    }
    else{
      t + rexp(n=1, rate = lamb)
    }
  }
}