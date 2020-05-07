#' function to generate the weights using the stick-breaking representation
#' 
#' @description Greet a person and appropriately capitalize their name.
#'
#' @param N Your name (character string; e.g. "john doe").
#' @param v Your name (character string; e.g. "john doe").
#'
#' @return A weights vector with the Stick-Breaking representation
#' @export
#'
weights = function(v)
	{
    N <- length(v)
		w = base::vector( mode = "numeric", length = N )
		
		w[1] = v[1]
		
		if( N == 1) { 
		  return(w)
		  }
		else
			{
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

we <- function(v){
  
}
