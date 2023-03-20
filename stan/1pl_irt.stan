

data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K_theta;  // number of population-level effects
  matrix[N, K_theta] X_theta;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K_theta] b_theta;  // population-level effects
  vector[K_beta] b_beta;  // population-level effects
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b_theta | 0, 1);
  lprior += normal_lpdf(b_beta | 0, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_theta = X_theta * b_theta;
    // initialize linear predictor term
    vector[N] nlp_beta = X_beta * b_beta;
    // initialize non-linear predictor term
    vector[N] mu;
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = Phi(nlp_theta[n] - nlp_beta[n]);
    }
    target += bernoulli_lpmf(Y | mu);
  }
  // priors including constants
  target += lprior;
}


  
