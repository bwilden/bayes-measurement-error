
data {
  int<lower=1> N;    
  array[N] real mean_ideal; // observed x's
  array[N] real sd_ideal;   // standard error estimate for each x
  vector[N] d_ideal;
  vector[N] vote_pct;   
}

parameters {
  real alpha;           
  real beta1;
  real beta2;
  real<lower=0> sigma;
  array[N] real ideal;       // true x's
  real<lower=0> tau;     // scale hyperparameter for true x
  real mu;               // location hyperparameter for true x
}

model {
  alpha ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  mu ~ normal(0, 1);
  to_vector(ideal) ~ normal(mu, tau);
  
  for (i in 1:N) {
    mean_ideal[i] ~ normal(ideal[i], sd_ideal[i]);
        
    vote_pct[i] ~ normal(alpha + beta1 * ideal[i] + beta2 * d_ideal, sigma);
  }
}
