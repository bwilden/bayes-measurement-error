
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
tar_source(here("R", "house_reelection_funcs.R"))

options(mc.cores = parallel::detectCores() - 1)

select <- dplyr::select

true_b = 1

list(
  # IRT simulation
  # tar_target(
  #   irt_data,
  #   sim_irt_data(n_groups = 200, n_items = 200)
  # ),
  # tar_target(
  #   fit_irt,
  #   fit_brms_irt(irt_data$dat |> 
  #                  filter(participate == 1))
  # ),
  # tar_target(
  #   irt_qis,
  #   summarise_irt_draws(fit_irt)
  # ),
  # tar_target(
  #   theta_check_plot,
  #   create_theta_check_plot(irt_data$dat, irt_qis)
  # ),
  # tar_target(
  #   theta_dist_plot,
  #   create_theta_dist_plot(irt_qis)
  # ),
  # tar_target(
  #   irt_combined_data,
  #   assemble_sim_irt_results(irt_data$dat, irt_qis, true_beta = 1)
  # ),
  # tar_stan_mcmc(
  #   me_irt,
  #   stan_file = here("stan", "sim", "me_cont.stan"),
  #   data = irt_combined_data$stan_list
  # ),
  # tar_stan_mcmc(
  #   no_me_irt,
  #   stan_file = here("stan", "sim", "no_me_cont.stan"),
  #   data = irt_combined_data$stan_list
  # ),
  # tar_target(
  #   irt_coef_plot,
  #   compare_coefs(me_irt_draws_me_cont, 
  #                 no_me_irt_draws_no_me_cont,
  #                 true = true_b)
  # ),
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
         stan_file = here("stan", "sim", "me_cont.stan"),
         model_name = "Measurement Error") |> 
      list_rbind()
  ),
  tar_target(
    no_me_cont,
    map2(map(cont_data, ~.x$stan_list),
         map(cont_data, ~.x$corr_x),
         fit_mcmc_cont,
         stan_file = here("stan", "sim", "no_me_cont.stan"),
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
    stan_file = here("stan", "sim", "skew.stan"),
    data = bias_data$stan_list
  ),
  tar_stan_mcmc(
    no_me_cont_bias,
    stan_file = here("stan", "sim", "no_me_cont.stan"),
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
    stan_file = here("stan", "sim", "skewt.stan"),
    data = list(N = 10000,
                x = sn::rsn(10000, xi = 0, omega = 1, alpha = 0.1))
  ),
  
  # House Re-election Analysis
  tar_target(
    raw_votes,
    here::here("data-raw", "Hall_votes.csv"),
    format = "file"
  ),
  tar_target(
    raw_legis,
    here::here("data-raw", "CELHouse93to117Classic-1.xls"),
    format = "file"
  ),
  tar_target(
    district_files,
    list.files(here::here("data-raw", "prcd"), pattern = "*.csv", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    candidate_vote_totals_file,
    here::here("data-raw", "1976-2020-house.csv")
  ),
  tar_target(
    district_pres_votes,
    pmap_dfr(tibble(district_file = district_files,
                    congress = 103:115),
             prep_district_presidential_votes)
  ),
  tar_target(
    votes_rc,
    prep_votes_rc(raw_votes, raw_legis, congress_list = 102:117)
  ),
  # tar_target(
  #   votes_irt,
  #   map(votes_rc$votes_list,
  #       fit_brms_irt)
  # ),
  # tar_target(
  #   votes_irt,
  #   fit_brms_irt(votes_rc$votes_list[[1]])
  # ),
  tar_target(
    votes_ideal,
    map(votes_rc$votes_list_rc,
        pscl::ideal,
        dropList = list(lop = NA),
        normalize = TRUE,
        .progress = TRUE,
        maxiter = 50000, burnin = 25000)
  ),
  tar_target(
    votes_ideal_dps,
    pmap_dfr(tibble(ideal_obj = votes_ideal, congress = 102:117),
             process_ideal_points,
             .progress = TRUE)
  ),
  tar_target(
    clean_legis,
    prep_legis(raw_legis)
  ),
  tar_target(
    clean_candidate_votes,
    prep_candidates(candidate_vote_totals_file)
  ),
  tar_target(
    legis_ideal,
    clean_candidate_votes |> 
      left_join(district_pres_votes, by = c("congress", "district", "state")) |> 
      left_join(clean_legis, by = c("congress", "party", "state", "district")) |> 
      left_join(votes_ideal_dps, by = c("icpsr", "congress")) |>
      na.omit() |>
      group_split(party)
  ),
  tar_stan_mcmc(
    dems_no_me,
    stan_file = here("stan", "application", "no_me.stan"),
    data = list(
      N = nrow(legis_ideal[[1]]),
      x_obs = legis_ideal[[1]]$mu,
      control = legis_ideal[[1]]$r_vote_pct,
      y = legis_ideal[[1]]$vote_pct)
  ),
  tar_stan_mcmc(
    dems_me,
    stan_file = here("stan", "application", "me.stan"),
    data = list(
      N = nrow(legis_ideal[[1]]),
      x_obs = legis_ideal[[1]]$mu,
      x_sd = legis_ideal[[1]]$sigma,
      control = legis_ideal[[1]]$r_vote_pct,
      y = legis_ideal[[1]]$vote_pct)
  ),
  tar_stan_mcmc(
    reps_no_me,
    stan_file = here("stan", "application", "no_me.stan"),
    data = list(
      N = nrow(legis_ideal[[2]]),
      x_obs = legis_ideal[[2]]$mu,
      control = legis_ideal[[2]]$r_vote_pct,
      y = legis_ideal[[2]]$vote_pct)
  ),
  tar_stan_mcmc(
    reps_me,
    stan_file = here("stan", "application", "me.stan"),
    data = list(
      N = nrow(legis_ideal[[2]]),
      x_obs = legis_ideal[[2]]$mu,
      x_sd = legis_ideal[[2]]$sigma,
      control = legis_ideal[[2]]$r_vote_pct,
      y = legis_ideal[[2]]$vote_pct)
  ),
  tar_stan_mcmc(
    dems_skew,
    stan_file = here("stan", "application", "skew.stan"),
    data = list(
      N = nrow(legis_ideal[[1]]),
      x_obs = legis_ideal[[1]]$mu,
      x_sd = legis_ideal[[1]]$omega,
      x_skew = legis_ideal[[1]]$alpha,
      control = legis_ideal[[1]]$r_vote_pct,
      y = legis_ideal[[1]]$vote_pct),
    init = 0
  ),
  tar_target(
    test_d,
    legis_ideal[[2]] |> 
      filter(alpha > -1)
  ),
  tar_stan_mcmc(
    reps_skew,
    stan_file = here("stan", "application", "skew.stan"),
    data = list(
      N = nrow(test_d),
      x_obs = test_d$mu,
      x_sd = test_d$omega,
      x_skew = test_d$alpha,
      control = test_d$r_vote_pct,
      y = test_d$vote_pct),
    init = 0
  ),
  tar_target(
    reelection_draws,
    assemble_reelection_draws(dems_no_me_draws_no_me,
                              dems_me_draws_me,
                              reps_no_me_draws_no_me,
                              reps_me_draws_me,
                              dems_skew_draws_skew,
                              reps_skew_draws_skew)
  ),
  tar_target(
    reelection_coef_plot,
    compare_reelection_coefs(reelection_draws)
  )
)
