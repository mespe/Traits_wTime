### Adding time to Traits model 
### July 31, 2019 
### JAGS : Don't know which beta corresponds to which pop/time measurement 
### STAN : Error 
### To do : Select out means of interest 
### To do : Model variance components as well as means 

Trait2017_data<-read.csv(file.choose())
HECO_data<-Trait2017_data[Trait2017_data$SPECIES=="HECO",]

HECO_SLA<-subset(HECO_data, select=c("POP_ID", "H_num","SLA.TOT"))
n=length(HECO_SLA)
M <- model.matrix(~POP_ID * H_num, data = HECO_SLA)
 
modelString="
model {

# Likelihood
 for (i in 1:n) {
 y[i]~dnorm(mean[i],tau)
 mean[i] <- inprod(beta[],M[i,])
 }
# Priors and derivatives
 for (i in 1:p) {
 beta[i] ~ dnorm(0, 1.0E-6) #prior
 }
 sigma ~ dgamma(0.001, 0.001)
 tau <- 1 / (sigma * sigma)
 }
 "
writeLines(modelString,con="SLA.txt")


M <- model.matrix(~POP_ID * H_num, data = HECO_SLA)
data.list <- with(HECO_SLA,
                    list(y=SLA.TOT,
                    M=M,
                    n=nrow(HECO_SLA),
                    p=ncol(M)))
 
params <- c("beta","sigma")
adaptSteps = 1000
burnInSteps = 2000
nChains = 3
numSavedSteps = 5000
thinSteps = 10
nIter = ceiling((numSavedSteps * thinSteps)/nChains)


library(R2jags)
jags.effects.simple.time <- system.time(
  data.r2jags <- jags(data=data.list,
                      inits=NULL,
                      parameters.to.save=params,
                      model.file="SLA.txt",
                      n.chains=3,
                      n.iter=nIter,
                      n.burnin=burnInSteps,
                      n.thin=thinSteps
  )
)

print(data.r2jags)

#
#
#
#
#
#
#
# STAN 
#
#
#
#
#
#

rstanString="
data {
int<lower=0> n;
int<lower=0> p;
vector[n] y;
matrix [n,p] M;
}
 parameters {
 vector[p] beta;
 real<lower=0> sigma;
 }
 model {
 vector[n] mu;
 
 #Priors
 beta ~ normal(0.01, 100000);
 sigma ~ uniform(0,100);
 
 #Likelihood
 mu <- M*beta;
 y ~ normal(mu, sigma);
 }
 "
 M <- model.matrix(~POP_ID * H_num, data = HECO_SLA)
 data.list <- list(y = HECO_SLA$SLA.TOT, M = M, n = nrow(HECO_SLA), p = ncol(M))
  
nChains = 3
burnInSteps = 3000
thinSteps = 10
numSavedSteps = 15000 #across all chains
nIter = ceiling(burnInSteps+(numSavedSteps * thinSteps)/nChains)
    
library(rstan)
rstan.simple.time <- system.time(
data.rstan <- stan(data=data.list,
                   model_code=rstanString,
                   chains=nChains,
                   iter=nIter,
                   warmup=burnInSteps,
                   thin=thinSteps,
                   save_dso=TRUE))


