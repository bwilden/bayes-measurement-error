data {
  int<lower=1> N;    
  array[N] real x_obs; 
  array[N] real<lower=0> x_sd;   
  array[N] real x_skew;
  array[N] real control;
  array[N] real y;
}

parameters {
  real alpha;
  real beta1;
  real beta2;            
  real<lower=0> sigma;
  array[N] real x;       
  real<lower=0> tau;
  real mu;
}

model {
  alpha ~ normal(50, 10);
  beta1 ~ normal(0, 5);
  beta2 ~ normal(0, 5);
  sigma ~ student_t(5, 0, 2);
  tau ~ student_t(5, 0, 2);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);

  for (i in 1:N) {
    x_obs[i] ~ skew_normal(x[i], x_sd[i], x_skew[i]);
    y[i] ~ normal(alpha + beta1 * x[i] + beta2 * control[i], sigma);
  }
}
