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
#' @examples
#' hello("james bond")
weights = function(N, v)
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
