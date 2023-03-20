
data {
  int<lower=1> N; 
  int<lower=1> K;  
  array[N, K] real<lower=0,upper=1> p;
  vector[N] y;
}

parameters {
  vector[K] mu;
  real<lower=0> sigma;
}

model {
  mu ~ normal(0, 2);
  sigma ~ student_t(3, 0, 2);
  
  
  
  
  
  

  array[K] real likelihood_terms;
  for(i in 1:N){
    for(j in 1:K){
      likelihood_terms[j] = log(p[i, j]) + normal_lpdf(y[i] | mu[j], sigma);
    }
    target += log_sum_exp(likelihood_terms);
  }
}
