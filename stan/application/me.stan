
data {
  int<lower=1> N;    
  array[N] real x_obs; // observed x's
  array[N] real<lower=0> x_sd;   // standard error estimate for each x
  vector[N] control;
  vector[N] y;   
}

parameters {
  real alpha;           
  real beta1;
  real beta2;
  real<lower=0> sigma;
  array[N] real x;       // true x's
  real<lower=0> tau;     // scale hyperparameter for true x
  real mu;               // location hyperparameter for true x
}

model {
  alpha ~ normal(0, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  // sigma ~ exponential(1);
  // tau ~ exponential(1);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);
  
  for (i in 1:N) {
    x_obs[i] ~ normal(x[i], x_sd[i]);
        
    y[i] ~ normal(alpha + beta1 * x[i] + beta2 * control[i], sigma);
  }
}
