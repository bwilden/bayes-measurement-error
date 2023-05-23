

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

assemble_reelection_draws <- function(dems_no_me_draws,
                                      dems_me_draws,
                                      reps_no_me_draws,
                                      reps_me_draws,
                                      dems_skew_draws,
                                      reps_skew_draws) {
  assembled_draws <- rbind(
    rbind(
      dems_me_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "Normal ME Model"),
      dems_no_me_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "MAP Model"),
      dems_skew_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "Skew-Normal ME Model")
    ) |> 
      mutate(party = "Democrat"),
    rbind(
      reps_me_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "Normal ME Model"),
      reps_no_me_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "MAP Model"),
      reps_skew_draws |> 
        dplyr::select("beta1") |> 
        mutate(model = "Skew-Normal ME Model")
    ) |> 
      mutate(party = "Republican")
  )
  return(assembled_draws)
}
