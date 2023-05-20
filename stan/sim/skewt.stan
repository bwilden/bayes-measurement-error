
data {
  int<lower=1> N;
  vector[N] x;
}
parameters {
  real alpha;
  real xi;
  real<lower=0> omega;
}
model {
  alpha ~ normal(0, 2);
  xi ~ normal(0, 2);
  omega ~ exponential(1);
  x ~ skew_normal(xi, omega, alpha);
}
