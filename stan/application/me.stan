
data {
  int<lower=1> N; 
  array[N] real x_obs; 
  array[N] real<lower=0> x_sd;   
  array[N] real control;
  vector[N] y;   
}

parameters {
  real beta0;
  real beta1;
  real beta2;
  real<lower=0> sigma_y;
  array[N] real x;   
  real<lower=0> tau;     
  real mu_x;               
}
model {
  mu_x ~ normal(0, 1);
  tau ~ cauchy(0, 3);
  beta0 ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  
  sigma_y ~ cauchy(0, 3);
  to_vector(x) ~ normal(mu_x, tau);
  
  for (i in 1:N) {
    x_obs[i] ~ normal(x[i], x_sd[i]);
    y[i] ~ normal(beta0 + beta1 * x[i] + beta2 * control[i], sigma_y);
  }


}
