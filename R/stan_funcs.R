
fit_mcmc_cont <- function(stan_file, data_list, corr_x, model_name) {
  fit <- fit_mcmc(stan_file = stan_file, data_list = data_list)
  
  coefs <- fit$summary(variables = "beta", "mean", ~quantile(.x, probs = c(.055, .945))) |> 
    mutate(corr_x := corr_x,
           model_name := model_name)
  
  return(coefs)
}


fit_brms_irt <- function(data_df) {
  fit <- brm(
    bf(yea ~ gamma * theta + beta,
       theta ~ 0 + icpsr,
       beta ~ 0 + rollnumber,
       gamma ~ 0 + republican + rollnumber,
       nl = TRUE),
    data = data_df,
    family = bernoulli(link = "probit"),
    prior = 
      prior(normal(0, 2), class = b, nlpar = beta) +
      prior(normal(0, 1), class = b, nlpar = theta) +
      prior(normal(0, 2), class = b, nlpar = gamma) +
      # prior(cauchy(0, 2), class = sd, nlpar = theta) +
      # prior(constant(1), class = sd, nlpar = gamma) +
      # prior(cauchy(0, 2), class = sd, nlpar = beta) +
      prior(constant(.5), class = b, coef = republican, nlpar = gamma),
    chains = 4,
    cores = 8,
    iter = 2000,
    backend = "cmdstanr",
    threads = 2
  )
  return(fit)
}


fit_mcmc <- function(stan_file, data_list) {
  mod <- cmdstan_model(stan_file)
  fit <- mod$sample(
    data = data_list,
    chains = 4,
    init = 0,
    threads_per_chain = 2
  )
  return(fit)
}



skew_inits <- function() {
  list(sigma = runif(1, .1, 1),
       tau = runif(1, .1, 1))
}
