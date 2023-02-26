
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
               "cmdstanr")
)

tar_source(here("R", "sim_funcs.R"))
tar_source(here("R", "stan_funcs.R"))
tar_source(here("R", "post-process_funcs.R"))

options(mc.cores = parallel::detectCores() - 1)


list(
  tar_target(
    cat_data,
    sim_categorical_data(N = 100,
                         turnout = lst(a = 0.7, b = 0.5, c = 0.3),
                         bias = 0)
  ),
  # tar_target(
  #   bin_data,
  #   sim_binary_data(N = 300,
  #                   turnout = lst(
  #                     black = 0.3,
  #                     white = 0.7
  #                   ),
  #                   bias = 0)
  # ),
  tar_target(
    cont_data,
    pmap(crossing(noise_x = c(.5),
                  noise_a = c(seq(0, 20, by = 0.5))),
         sim_cont_data,
         N = 500,
         true_b = 0,
         bias = 5)
  ),
  tar_stan_mcmc(
    me_cont,
    stan_files = here("stan", "me_cont.stan"),
    data = cont_data[[2]]$stan_list,
    threads_per_chain = 2
  ),
  tar_stan_mcmc(
    no_me_cont,
    stan_files = here("stan", "no_me_cont.stan"),
    data = cont_data[[2]]$stan_list,
    threads_per_chain = 2
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
                       true = cont_data[[1]]$true_b)
  ),
  tar_stan_mcmc(
    me_cat,
    stan_files = here("stan", "me_cat.stan"),
    data = cat_data$stan_list,
    threads_per_chain = 2
  )
  # tar_stan_mcmc(
  #   meas_cat,
  #   stan_files = here("stan", "no_me_cat.stan"),
  #   data = cat_data$stan_list,
  #   threads_per_chain = 2
  # ),
  # tar_target(
  #   cat_coef_plot,
  #   compare_cat_coefs(me = me_cat_draws_me_cat,
  #                     meas = meas_cat_draws_no_me_cat)
  # )
)
