
fit_mcmc_cont <- function(stan_file, data_list, corr_x, model_name) {
  mod <- cmdstan_model(stan_file)
  fit <- mod$sample(
    data = data_list,
    chains = 4,
    init = 0,
    threads_per_chain = 2
  )
  # draws <- fit$draws(format = "df") |> 
  #   as_tibble() |> 
  #   mutate(fit_name := as.character(fit_name))
  
  coefs <- fit$summary(variables = "beta", "mean", ~quantile(.x, probs = c(.055, .945))) |> 
    mutate(corr_x := corr_x,
           model_name := model_name)
  
  return(coefs)
}
# fit_mcmc(here("stan", "me_cont.stan"), cont_data[[2]]$stan_list) |> View()
