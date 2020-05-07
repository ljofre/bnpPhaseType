#' Function to take a random draw from the p(phi_j | ....)
#' 
#' @title posterior distribution
#' @description Greet a person and appropriately capitalize their name.
#'
#' @param y Your name (character string; e.g. "john doe").
#' @param lambda Your name (character string; e.g. "john doe").
#' @param aa Your name (character string; e.g. "john doe").
#' @param bb Your name (character string; e.g. "john doe").
#' @param R_ante Your name (character string; e.g. "john doe").
#' @importFrom  truncnorm rtruncnorm dtruncnorm
#' @importFrom stats dgamma rbeta rgamma runif
phi_posterior = function( y, lambda, aa, bb, R_ante )
	{	
		cc = 0.1 ; nn = 2
		nj= length( y )
		KR_ante = ceiling( R_ante )
		Rtemp = vector( mode = "numeric", length = nn )
		Rtemp[1] = R_ante
		for ( t in 2 : nn )
		{		
		R_aste = truncnorm::rtruncnorm( n = 1, a = 0, b = Inf, mean = R_ante , sd = cc )
		KR_aste = ceiling( R_aste )
		ln_R_aste = nj * KR_aste * log( lambda ) + ( KR_aste-1 ) * sum( log( y ) ) - log( nj ) - lfactorial( (KR_aste - 1) ) + ( aa - 1 ) * log( R_aste ) - bb * R_aste
		ln_R_ante = nj * KR_ante * log( lambda ) + ( KR_ante-1 ) * sum( log( y ) ) - log( nj ) - lfactorial( (KR_ante - 1) ) + ( aa - 1 ) * log( R_ante ) - bb * R_ante
		temp1 = ln_R_aste - log( dtruncnorm( x = R_aste, a = 0, b = Inf, mean = R_ante, sd = cc ) )
		temp2 = ln_R_ante - log( dtruncnorm( x = R_ante, a = 0, b = Inf, mean = R_aste, sd = cc ) )		
		lnr = temp1 - temp2
		prob = min( c( exp(lnr), 1 ) )
		u = runif( 1 )
		if( prob > u ){ Rtemp[t] = R_aste }
		else { Rtemp[t] = R_ante }
		R_ante = Rtemp[t]
		KR_ante = ceiling( R_ante )
		} 
	return( Rtemp[2] )	
	}
