
data {
  int<lower=1> N;    
  array[N] real x_meas; // observed x's
  array[N] real x_sd;   // standard error estimate for each x
  vector[N] y;   
  int skew;
}

parameters {
  real alpha;           
  real beta;            
  real<lower=0> sigma;
  array[N] real x;       // true x's
  real<lower=0> tau;     // scale hyperparameter for true x
  real mu;               // location hyperparameter for true x
  array[N] real lambda;
  real<lower=0> xi;
}

model {
  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  tau ~ student_t(3, 0, 2);
  mu ~ normal(0, 1);
  to_vector(x) ~ normal(mu, tau);
  to_vector(lambda) ~ normal(0, 1);
  xi ~ student_t(3, 0, 2);
  
  for (i in 1:N) {
    if (skew == 1) {
      mean(x_meas) - x_meas[i] ~ normal(lambda[i], xi);
      x_meas[i] ~ skew_normal(x[i], x_sd[i], lambda[i]);
    } else {
      x_meas[i] ~ normal(x[i], x_sd[i]);
    }
        
    y[i] ~ normal(alpha + beta * x[i], sigma);
  }
}

