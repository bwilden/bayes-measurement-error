
data {
  int<lower=1> N;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
  array[N] real x;
  real<lower=0> tau;
  real mu;
  array[N] real<lower=0> x_sd;
  array[N] real x_skew;
  array[N] real control;
}

model {
  alpha ~ normal(50, 10);
  beta1 ~ normal(0, 5);
  beta2 ~ normal(0, 5);
  sigma ~ student_t(30, 0, 2);
  tau ~ student_t(30, 0, 2);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);
  x_sd ~ exponential(5);
  x_skew ~ normal(0, 1);
  control ~ normal(40, 10);
}

generated quantities {
  // Simulate new x values based on the prior distribution
  array[N] real x_pred;
  for (i in 1:N) {
    x_pred[i] = normal_rng(mu, tau);
  }
  
  // Simulate new x_obs values based on the prior distribution
  array[N] real x_obs_pred;
  for (i in 1:N) {
    x_obs_pred[i] = skew_normal_rng(x_pred[i], x_sd[i], x_skew[i]);
  }
  
  // Simulate new y values based on the prior distribution
  array[N] real y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(alpha + beta1 * x_pred[i] + beta2 * control[i], sigma);
  }
}
