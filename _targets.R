
library(targets)
library(stantargets)
library(tarchetypes)
library(here)

# Set target options:
tar_option_set(
  packages = c("tidyverse",
               "MCMCpack",
               "tidybayes",
               "ggdist",
               "MetBrewer",
               "cmdstanr",
               "brms",
               "tidybayes",
               "sn")
)

tar_source(here("R", "sim_funcs.R"))
tar_source(here("R", "stan_funcs.R"))
tar_source(here("R", "post-process_funcs.R"))
tar_source(here("R", "plot_funcs.R"))

options(mc.cores = parallel::detectCores() - 1)

true_b = 1

list(
  # IRT simulation
  tar_target(
    irt_data,
    sim_irt_data(n_groups = 200, n_items = 200)
  ),
  tar_target(
    fit_irt,
    fit_brms_irt(irt_data$dat |> 
                   filter(participate == 1))
  ),
  tar_target(
    irt_qis,
    summarise_irt_draws(fit_irt)
  ),
  tar_target(
    theta_check_plot,
    create_theta_check_plot(irt_data$dat, irt_qis)
  ),
  tar_target(
    theta_dist_plot,
    create_theta_dist_plot(irt_qis)
  ),
  tar_target(
    irt_combined_data,
    assemble_sim_irt_results(irt_data$dat, irt_qis, true_beta = 1)
  ),
  tar_stan_mcmc(
    me_irt,
    stan_file = here("stan", "me_cont.stan"),
    data = irt_combined_data$stan_list
  ),
  tar_stan_mcmc(
    no_me_irt,
    stan_file = here("stan", "no_me_cont.stan"),
    data = irt_combined_data$stan_list
  ),
  tar_target(
    irt_coef_plot,
    compare_coefs(me_irt_draws_me_cont, 
                  no_me_irt_draws_no_me_cont,
                  true = true_b)
  ),
  # Random noise ME
  tar_target(
    cont_data,
    map(c(seq(0, 2, by = .1), 2.5, 3, 4, 5, 10, 20),
         sim_cont_data,
         N = 300,
         true_b = true_b)
  ),
  tar_target(
    me_cont,
    map2(map(cont_data, ~.x$stan_list),
         map(cont_data, ~.x$corr_x),
         fit_mcmc_cont,
         stan_file = here("stan", "me_cont.stan"),
         model_name = "Measurement Error") |> 
      list_rbind()
  ),
  tar_target(
    no_me_cont,
    map2(map(cont_data, ~.x$stan_list),
         map(cont_data, ~.x$corr_x),
         fit_mcmc_cont,
         stan_file = here("stan", "no_me_cont.stan"),
         model_name = "Mean Values Only") |> 
      list_rbind()
  ),
  tar_target(
    cont_coef_plot,
    compare_cont_coefs(me = me_cont,
                       meas = no_me_cont,
                       true = true_b)
  ),
  # Bias ME
  tar_target(
    bias_data,
    sim_bias_dist_data(1000, true_b = 0, bias = 10)
  ),
  tar_stan_mcmc(
    me_cont_bias,
    stan_file = here("stan", "skew.stan"),
    data = bias_data$stan_list
  ),
  tar_stan_mcmc(
    no_me_cont_bias,
    stan_file = here("stan", "no_me_cont.stan"),
    data = bias_data$stan_list
  ),
  tar_target(
    bias_coef_plot,
    compare_coefs(me_cont_bias_draws_skew, 
                  no_me_cont_bias_draws_no_me_cont,
                  true = 0)
  ),
  tar_stan_mcmc(
    skew_test,
    stan_file = here("stan", "skewt.stan"),
    data = list(N = 10000,
                x = sn::rsn(10000, xi = 0, omega = 1, alpha = 0.1))
  )
)
