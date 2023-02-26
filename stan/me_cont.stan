
data {
  int<lower=0> N;    
  array[N] real x_meas;     
  array[N] real x_sd;
  vector[N] y;        
}

parameters {
  real alpha;           
  real beta;            
  real<lower=0> sigma;
  real<lower=0> tau;
  real mu;
  array[N] real x;  
}

model {
  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  mu ~ normal(0, 1);
  
  to_vector(x) ~ normal(mu, tau);
  for (i in 1:N) {
    x_meas[i] ~ normal(x[i], x_sd[i]);
    y[i] ~ normal(alpha + beta * x[i], sigma);
  }
}

