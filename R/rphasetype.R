library(MCMCpack)
library(mcompanion)


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


rPhDistribution <- function(n, alpha=alpha, T=T)
{
  # uniformization
  q <- length(alpha)
  lamb <- max(-diag(T))
  P = (1/lamb)*T + diag(q)
  replicate(n, rph(alpha=alpha, lamb = lamb, P=P, t=0))
}