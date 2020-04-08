mcmcErlangMix = function( Y, a, b, aa, bb, alpha, beta, nscan, nburn, nskip )
{   

# Fixed values
n = length(Y)
ii = 1 : n
sumy = sum(Y)
indexprint = c(2, seq(1001, nscan, 1000))
lengthindex = length(indexprint)
jj = 1:lengthindex

# Grid
tau.grid = seq( 0, 1.2*max(Y), 0.1 )
n.grid = length( tau.grid )
nfil = ( ( nscan - nburn ) / nskip )+1
fys = matrix( NA, nrow = nfil, ncol = n.grid )
cont = 0

# Set inits
M = 1
N = 10
d = sample( x = 1 : N , size = n, replace = TRUE) 
v = rbeta( n = N, shape1 = 1, shape2 = M )
temp = weights(N = N, v = v)
w = c( temp, ( 1 - sum( temp ) ) )
N = N + 1
R = rgamma( n = N, shape = aa, rate = bb )
KR = ceiling( R )
lambda = 10
u = vector( mode = "numeric", length = n )
for( i in 1 : n )
	{
	u[i] = runif( n = 1, min = 0, max= w[d[i]])
	}
k1 = 1 : N

lambdaM = vector( mode = "numeric", length = nscan )
NM = vector( mode = "numeric", length = nscan )
MM = vector( mode = "numeric", length = nscan )
phases = vector( mode = "numeric", length = nscan )
lambdaM[1] = lambda
NM[1] = N
MM[1] = M

for ( k in 2 : nscan )
{
# Step 1: Updating d and N
Nante = N
NN = vector( mode = "numeric", length = n)
	for ( i  in 1: n )
		{
			cuales1 = ( w > u[i] ) * k1
			kk1 = k1[cuales1]
			NN[i] = max( kk1 )
			KRtemp = KR[cuales1]
			den1 = dgamma( x = Y[i], shape = KRtemp, rate = lambda )
			prob1 = den1 / sum(den1)
			m1 = length(kk1)
			if ( m1 == 1 ) {d[i] = kk1 }
			else {d[i] = sample( x = kk1, size = 1, replace = FALSE, prob = prob1 )}
		}
N = max(NN)


# Step 2: Updating the atoms phi_j, j=1,...,N
R_ante = R
R = vector( mode = "numeric", length = N )
KR = vector( mode = "numeric", length = N )
cardinality = vector( mode = "numeric", length = N )
for( j in 1 : N )
	{
		index = ( d == j ) * 1	
		cardij = sum( index )
		indexi = index * ii
		cardinality[j] = cardij
		Yi = Y[indexi]
		if( cardij == 0 ) { R[j] = rgamma( n = 1, shape = aa, rate = bb ); KR[j] = ceiling( R[j] ) }
		else
			{
			R[j] = phi_posterior( Yi, lambda = lambda, aa = aa, bb = bb, R_ante[j] )
			KR[j] = ceiling( R[j] )
			}
	}
phases[k] = max( KR )

# Step 3: Updating the parameter lambda

shape = a + sum( KR * cardinality)
rate = b + sumy
lambda = rgamma( n = 1, shape = shape, rate = rate )
lambdaM[k] = lambda

# Step 4: Updating the weigths
v = vector( mode = "numeric", length = N ) 
for( j in 1 : N )
	{
		if( cardinality[j] == 0 ) {v[j] = rbeta( n = 1, shape1 = 1, shape2 = M ) }
		else 
			{
				aj = 1 + cardinality[j]
				bj = M + sum( (d > j)*1 )
				v[j] = rbeta( n = 1, shape1 = aj, shape2 = bj )
			}		
	}

temp2 = weights(N = N, v = v)
w = c( temp2, ( 1 - sum( temp2 ) ) )
temp3 = rgamma( n = 1, shape = aa, rate = bb )
R = c( R, temp3 )
KR = c( KR, ceiling( temp3 ) )
N = N + 1
k1 = 1 : N
NM[k] = N

# Step 5: Updating the latent variables u_i, i =1,...,n

u = vector( mode = "numeric", length = n )
for( i in 1 : n )
	{
	u[i] = runif( n = 1, min = 0, max= w[d[i]])
	}
	
# Step 6: Updating M as in Escobar and West 1995

nclus = length( unique( d ) )
eta = rbeta( n = 1, shape1 = ( M + 1 ) , shape2 = n )
omega = ( alpha + nclus - 1 ) / ( n * ( beta - log( eta ) ) + ( alpha + nclus - 1) )
uniforme1 = runif( 1 )
if ( omega >= uniforme1 ){ 
  M = rgamma( n=1, shape = ( alpha + nclus ), rate = ( beta - log( eta ) ) ) 
  }
else { 
  M = rgamma( n = 1, shape = ( alpha + nclus - 1 ), rate = ( beta - log( eta ) ) ) 
}

MM[k] = M

# density estimation
if( k >= nburn )
	{
		clave = k - round( k / nskip )* nskip
		if( clave == 0)	
		{
			cont = cont + 1
		sumtemp1 = 0
		for( j in 1 : N )
			{
     		sumtemp1 = sumtemp1 + w[j]*dgamma( x = tau.grid, shape = KR[j], rate = lambda )
			}
		fys[cont,] = sumtemp1
		
		}
	}


if( sum((k==indexprint)*1)==1)
{print(paste0("running iterations ", indexprint[max((k==indexprint)*jj)]-1, " to ", max((k==indexprint)*jj)*1000, " of ", nscan))}
}
return(fys)	
}   
