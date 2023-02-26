
data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of categories in the predictor variable
  vector[N] y; // response variable (binary)
  int<lower=1, upper=K> x_meas[N]; // predictor variable
}

parameters {
  vector[K] beta; 
  real<lower=0> sigma;
}

model {
  beta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  
  y ~ normal(beta[x_meas], sigma);
}

