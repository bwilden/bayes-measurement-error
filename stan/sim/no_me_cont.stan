
data {
  int<lower=0> N;    
  vector[N] x_meas;     
  vector[N] y;        
}

parameters {
  real alpha;           
  real beta;           
  real<lower=0> sigma;  
}

model {
  
  y ~ normal(alpha + beta * x_meas, sigma);

  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
}
