

summarise_irt_draws <- function(irt_model) {
  draws <- irt_model |> 
    spread_draws(r_group__theta[group,]) |> 
    summarise_draws() |> 
    mutate(group = as.character(group))
  return(draws)
}

assemble_sim_irt_results <- function(sim_data, irt_qis, true_beta) {
  sim_data = sim_data |> 
    dplyr::select(group, theta_i, partic_level, type) |> 
    distinct()
  
  dat <- sim_data |> 
    left_join(irt_qis, by = "group") |> 
    mutate(y = rnorm(n(), theta_i * true_beta)) |> 
    arrange(mean) |> 
    rownames_to_column()
  
  stan_list <- list(
    N = nrow(dat),
    x_meas = dat$mean,
    x_sd = dat$sd,
    y = dat$y,
    skew = 1
  )
  
  return(lst(dat, stan_list))
}
# summarise_irt_draws(fit_irt)
# ranef(fit_irt)$group[, , "theta_Intercept"] |> 
#   as.data.frame() |> 
#   rownames_to_column()


# fit_irt |> 
#   spread_draws(r_group__theta[group,]) |> 
#   filter(group %in% 1:10) |> 
#   ggplot(aes(x = r_group__theta, y = group)) +
#   geom_halfeyeh()
