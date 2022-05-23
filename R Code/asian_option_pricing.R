# Script for Monte-Carlo Simulation of Asian Option
set.seed(123456)
# Initial values and derived constants
K = 6500; iv = 0.2652 # Simulation one
# K = 5500; iv = 0.3433 # Simulation two
s0 = 6289.7; rf = 0.0624; dy = 0.0242; ttm = 0.5; obs = 125
dt = ttm/obs
drift = (rf-dy-iv^2/2)*dt
vsqrdt = iv*dt^0.5

putval = callval = NULL
for (i in 1:25000){
  random = rnorm(obs) # create cumulative sum of random numbers
  # Spot price evolution for positive and negative random numbers
  spot = s0*exp(drift*c(1:obs)+vsqrdt*cumsum(random)) 
  spot_neg = s0*exp(drift*c(1:obs)+vsqrdt*cumsum(-random))
  # Compute call values
  callval = c(callval, max(mean(spot)-K,0)*exp(-rf*ttm))
  callval = c(callval, max(mean(spot_neg)-K,0)*exp(-rf*ttm))
  # Compute Put values
  putval = c(putval, max(K-mean(spot),0)*exp(-rf*ttm))
  putval = c(putval, max(K-mean(spot_neg),0)*exp(-rf*ttm))
}
mean(callval)
mean(putval)