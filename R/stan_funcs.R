
fit_mcmc_cont <- function(stan_file, data_list, corr_x, model_name) {
  fit <- fit_mcmc(stan_file = stan_file, data_list = data_list)
  
  coefs <- fit$summary(variables = "beta", "mean", ~quantile(.x, probs = c(.055, .945))) |> 
    mutate(corr_x := corr_x,
           model_name := model_name)
  
  return(coefs)
}


fit_brms_irt <- function(data_df) {
  fit <- brm(
    bf(y ~ gamma * theta + beta,
       theta ~ (1 | group),
       beta ~ (1 | item),
       gamma ~ type + (1 | item),
       nl = TRUE),
    data = data_df,
    family = bernoulli(link = "probit"),
    prior = prior(normal(0, 1), class = b, nlpar = beta) +
      prior(normal(0, 1), class = b, nlpar = theta) +
      prior(normal(0, 1), class = b, nlpar = gamma) +
      prior(exponential(2), class = sd, nlpar = theta) +
      prior(exponential(2), class = sd, nlpar = gamma) +
      prior(exponential(2), class = sd, nlpar = beta) +
      prior(lognormal(0, .5), class = b, coef = typeb, nlpar = gamma),
    chains = 4,
    cores = 4,
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
