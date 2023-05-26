
data {
  int<lower=1> N; 
  int<lower=1> N_legis;
  int<lower=1, upper=N_legis> legis[N];
  int<lower=1> N_year;
  int<lower=1, upper=N_year> year[N];
  array[N] real x_obs; // observed x's
  array[N] real<lower=0> x_sd;   // standard error estimate for each x
  vector[N] control;
  vector[N] y;   
}

parameters {
  vector[N_legis] alpha_legis;
  vector[N_year] alpha_year;          
  real beta1;
  real beta2;
  real<lower=0> sigma_legis;
  real<lower=0> sigma_year;
  real<lower=0> sigma_y;
  real mu_legis;
  real mu_year;
  vector[N] x;       // true x's
  real<lower=0> tau;     // scale hyperparameter for true x
  real mu_x;               // location hyperparameter for true x
}

model {
  mu_x ~ normal(0, 1);
  tau ~ cauchy(0, 3);
  to_vector(x) ~ normal(mu_x, tau);
  
  vector[N] mu;
  
  mu = alpha_legis[legis] + alpha_year[year] + beta1 * x + beta2 * control;
  x_obs ~ normal(x, x_sd);

  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  
  mu_legis ~ normal(0, 1);
  mu_year ~ normal(0, 1);
  sigma_legis ~ cauchy(0, 3);
  sigma_year ~ cauchy(0, 3);
  sigma_y ~ cauchy(0, 3);
  
  alpha_legis ~ normal(mu_legis, sigma_legis);
  alpha_year ~ normal(mu_year, sigma_year);
  
  y ~ normal(mu, sigma_y);
}
