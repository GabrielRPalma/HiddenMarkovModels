####################################################################################################
###
### File:    02_poisson_HMM.R
### Purpose: Here, the estimation of parameters of a Poisson Hidden Markov Model based on Zucchini, 2009.
### Authors: Gabriel Palma
### Date:    01/04/2022
###
####################################################################################################

# Loading the packages --------- 
source('00_source.r')

#  --------- 
alpha <- delta*dpois(x[1],lambda)
lscale <- log(sum(alpha))
alpha <- alpha/sum(alpha)

for (i in 2:T) {
  alpha <- alpha %*% Gamma*dpois(x[i],lambda)
  lscale <- lscale+log(sum(alpha))
  alpha <- alpha/sum(alpha)
} 
lscale