
data {
  int<lower=1> N;    
  vector[N] x_obs;  
  vector[N] control;
  vector[N] y;        
}

parameters {
  real alpha;           
  real beta1; 
  real beta2;
  real<lower=0> sigma;  
}

model {
  alpha ~ normal(50, 5);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  
  y ~ normal(alpha + beta1 * x_obs + beta2 * control, sigma);
}
