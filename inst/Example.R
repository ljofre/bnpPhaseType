
data=read.table(file="./data_example.csv", sep=",", header=F)
y = data$V1
hist(y, breaks = 50)

# Libraries 

library(MCMCpack)
library(truncnorm)
library(GB2)
library(GIGrvg)
library(Matrix)
library(FAdist)

# External functions
source("weights.R")
source("phi_posterior.R")
source("mcmcErlangMix.R")

fys = mcmcErlangMix( Y=y, a=0.1, b=0.1, aa=2, bb=0.1, alpha=1, beta=1, nscan=10000, nburn=2000, nskip=8 )


# Grid
tau.grid = seq( 0, 1.2*max(y), 0.1 )

# True density
p1=0.7 
p2=0.3
fyr=p1*dgig(tau.grid, chi = 1, psi = 2, lambda = 12.0)+p2*dgig(tau.grid, chi = 1, psi = 2, lambda = 30.0)

# Estimated density
p2.5=function(x){quantile(x, probs=0.025)}
p97.5=function(x){quantile(x, probs=0.975)}
meanfy=apply(fys, 2, mean)
linffy=apply(fys, 2, p2.5)
lsupfy=apply(fys, 2, p97.5)
plot(tau.grid, meanfy, type="l", ylim=c(0,1.2*max(fyr)), xlab="y", ylab="fy")
polygon(x=c(tau.grid,rev(tau.grid)), y=c(linffy,rev(lsupfy)), lty=1, density=-1, col="grey", border="grey")
polygon(x=c(tau.grid,rev(tau.grid)), y=c(linffy,rev(lsupfy)), lty=1, density=-1, col="grey", border="grey")
lines(tau.grid, meanfy, lty=2)
lines(tau.grid, fyr, lty=1)
