library(microbenchmark)

# Fast version 
v = rbeta(10000, 1, 3)
stick_breaking_process = function(v) 
{ 
  num_weights = length(v)
  remaining_stick_lengths = c(1, cumprod(1 - v))[1:num_weights] 
  weights = remaining_stick_lengths * v  
  weights
}

microbenchmark(w1 <- stick_breaking_process(v))
sum(w1)

# function to generate the weights using the stick-breaking representation
weights = function( N, v)
{
  w = vector( mode = "numeric", length = N )
  if( N == 1) { w[1] = v[1] }
  else
  {
    w[1] = v[1]
    for ( j in 2 : N )
    {
      w[j] = v[j]
      for ( l in 1 : (j-1) )
      {
        w[j] = w[j] * ( 1 - v[l] )
      }
    }
  }
  return( w )
}

microbenchmark(w2 = weights(N=1000, v=v))
sum(w2)

sum((round(w1,15)==round(w2,15))*1)