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
 
  //Priors
  beta ~ normal(0, 10);
  sigma ~ normal(0,100);
 
  //Likelihood
  mu = M * beta;
  y ~ normal(mu, sigma);
}
