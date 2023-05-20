
data {
  int<lower=1> N;    
  array[N] real x_meas; 
  array[N] real<lower=0> x_sd;   
  array[N] real x_skew;
  vector[N] y;
}

parameters {
  real beta0;           
  real beta;            
  real<lower=0> sigma;
  array[N] real x;       
  real<lower=0> tau;
  real mu;
}

model {
  beta0 ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);
  
  for (i in 1:N) {
    x_meas[i] ~ skew_normal(x[i], x_sd[i], x_skew[i]);
        
    y[i] ~ normal(beta0 + beta * x[i], sigma);
  }
}

