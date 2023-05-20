
data {
  int<lower=1> N;    
  vector[N] mean_ideal;  
  vector[N] d_ideal;
  vector[N] vote_pct;        
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
  
  vote_pct ~ normal(alpha + beta1 * mean_ideal + beta2 * d_ideal, sigma);
}
