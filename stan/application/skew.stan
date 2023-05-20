
data {
  int<lower=1> N;    
  array[N] real mean_ideal; 
  array[N] real<lower=0> sd_ideal;   
  array[N] real skew_ideal;
  array[N] real d_ideal;
  vector[N] vote_pct;
}

parameters {
  real alpha;
  real beta1;
  real beta2;            
  real<lower=0> sigma;
  array[N] real ideal;       
  real<lower=0> tau;
  real mu;
}

model {
  alpha ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);
  
  for (i in 1:N) {
    mean_ideal[i] ~ skew_normal(ideal[i], sd_ideal[i], skew_ideal[i]);
        
    vote_pct[i] ~ normal(alpha + beta1 * ideal[i] + beta2 * d_ideal[i], sigma);
  }
}
