### Adding time to Traits model 
### July 31, 2019 
### JAGS : Don't know which beta corresponds to which pop/time measurement 
### STAN : Error 
### To do : Select out means of interest 
### To do : Model variance components as well as means 

library(R2jags)

Trait2017_data<-read.csv("data/HECO_ELTR_PLPA_VUOC.csv")

jags_model = "src/SLA.txt"
HECO_data<-Trait2017_data[Trait2017_data$SPECIES=="HECO",]

HECO_SLA<-subset(HECO_data, select=c("POP_ID", "H_num","SLA.TOT"))
n=length(HECO_SLA)
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
mod = stan_model("src/Traits.stan")

M <- model.matrix(~POP_ID * H_num, data = HECO_SLA)

## Stan does not handle NAs

missingY = is.na(HECO_SLA$SLA.TOT)

## Remove NAs
M = M[!missingY,]
y = HECO_SLA$SLA.TOT[!missingY]

data.list <- list(y = y,
                  M = M,
                  n = nrow(M),
                  p = ncol(M))
  
# don't do this
## nChains = 3
## burnInSteps = 3000
## thinSteps = 10
## numSavedSteps = 15000 #across all chains
## nIter = ceiling(burnInSteps+(numSavedSteps * thinSteps)/nChains)
    
library(rstan)
rstan.simple.time <- system.time(
    data.rstan <- sampling(mod, data=data.list))


