data {
  int<lower=1> N;
  array[N] real x_obs; 
  array[N] real<lower=0> x_sd;   
  array[N] real x_skew;
  vector[N] control;
  array[N] real y;
}

parameters {
  real beta0;
  real beta1;
  real beta2;            
  real<lower=0> sigma_y;
  vector[N] x;   
  real<lower=0> tau;     
  real mu_x;  
}

model {
  mu_x ~ normal(0, 2);
  tau ~ cauchy(0, 3);
  to_vector(x) ~ normal(mu_x, tau);
  
  vector[N] mu;
  
  mu = beta0 + beta1 * x + beta2 * control;
  x_obs ~ skew_normal(x, x_sd, x_skew);

  beta0 ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  
  sigma_y ~ cauchy(0, 3);
  
  y ~ normal(mu, sigma_y);
}
