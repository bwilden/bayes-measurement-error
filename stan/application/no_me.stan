
data {
  int<lower=0> N;  
  vector[N] x_obs;  
  vector[N] control;
  vector[N] y;        
}

parameters {
  real beta0;
  real beta1; 
  real beta2;
  real<lower=0> sigma_y;
}

model {
  vector[N] mu;
  
  mu = beta0 + beta1 * x_obs + beta2 * control;

  beta0 ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  
  sigma_y ~ cauchy(0, 3);
  
  y ~ normal(mu, sigma_y);
}
