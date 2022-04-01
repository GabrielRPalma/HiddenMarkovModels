####################################################################################################
###
### File:    01_independen_mixed_models.R
### Purpose: Here, I present ideas on how to estimate parameters from indepenent mixed models based on Zucchini, 2009.
### Authors: Gabriel Palma
### Date:    01/04/2022
###
####################################################################################################

# Loading the packages --------- 
source('00_source.r')

# Loading the data ----------
earthquake_frequency <- c(13, 14, 8, 10, 16, 26, 32, 27, 18, 
                     32, 36, 24, 22, 23, 22, 18, 25, 21, 
                     21, 14, 8, 11, 14, 23, 18, 17, 19, 20,
                     22, 19, 13, 26, 13, 14, 22, 24, 21, 22, 
                     26, 21, 23, 24, 27, 41, 31, 27, 35, 26, 
                     28, 36, 39, 21, 17, 22, 17, 19, 15, 34, 
                     10, 15, 22, 18, 15, 20, 15, 22, 19, 16,
                     30, 27, 29, 23, 20, 16, 21, 21, 25, 16,
                     18, 15, 18, 14, 10, 15, 8, 15, 6, 11, 
                     8, 7, 18, 16 , 13, 12, 13, 20, 15, 16, 
                     12, 18, 15, 16, 13, 15, 16, 11, 11)
data <- data.frame(earthquakes = earthquake_frequency, 
                   time = seq(1, length(earthquake_frequency), 1))

# Data visualisation ------
ggplot(data = data, mapping = aes(x = time, y = earthquakes))+
  geom_line() + theme_new()

# Function to compute -log(likelihood)
mllk <- function(wpar,x){ zzz <- w2n(wpar)
  -sum(log(outer(x,zzz$lambda,dpois)%*%zzz$delta)) }

# Function to transform natural to working parameters
n2w  <- function(lambda,delta){
  log(c(lambda,delta[-1]/(1-sum(delta[-1]))))
  }

# Function to transform working to natural parameters
w2n  <- function(wpar){
    m <- (length(wpar)+1)/2
    lambda <- exp(wpar[1:m])
    delta  <- exp(c(0,wpar[(m+1):(2*m-1)]))
    return(list(lambda=lambda,delta=delta/sum(delta)))
    }
# Read data, specify starting values, minimize -log(likelihood),
# and transform to natural parameters
x <- earthquake_frequency
wpar <- n2w(c(10,20,25,30),c(1,1,1,1)/4)
w2n(nlm(mllk,wpar,x)$estimate)

# Transform Poisson natural parameters to working parameters
pois.HMM.pn2pw <- function(m,lambda,gamma,delta=NULL,stationary=TRUE)
{
  tlambda <- log(lambda)
  foo     <- log(gamma/diag(gamma))
  tgamma  <- as.vector(foo[!diag(m)])
  if(stationary) {tdelta <-NULL} else {tdelta<-log(delta[-1]/delta[1])}
  parvect <- c(tlambda,tgamma,tdelta)
  return(parvect)
}
# Transform Poisson working parameters to natural parameters
pois.HMM.pw2pn <- function(m,parvect,stationary=TRUE)
{
  lambda        <- exp(parvect[1:m])
  gamma         <- diag(m)
  gamma[!gamma] <- exp(parvect[(m+1):(m*m)])
  gamma         <- gamma/apply(gamma,1,sum)
  if(stationary) {delta<-solve(t(diag(m)-gamma+1),rep(1,m))} else
  {foo<-c(1,exp(parvect[(m*m+1):(m*m+m-1)]))
  delta<-foo/sum(foo)}
  return(list(lambda=lambda,gamma=gamma,delta=delta))
}
