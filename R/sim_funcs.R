

sim_categorical_data = function(N, turnout, bias) {
  x = sample(c("a", "b", "c"), N, replace = TRUE)
  confound = rbinom(N, 1, 0.5)
  
  y = NULL
  probs = tibble(pr_a = NA, 
                 pr_b = NA,
                 pr_c = NA)
  for (i in seq_len(N)) {
    if (x[i] == "a") {
      probs[i, ] = rdirichlet(1, c(3, 1, 1))
      y[i] = rbinom(1, 1, turnout$a)
    } else if (x[i] == "b") {
      probs[i, ] = rdirichlet(1, c(1, 3, 1))
      y[i] = rbinom(1, 1, turnout$b)
    } else if (x[i] == "c") {
      probs[i, ] = rdirichlet(1, c(1, 1, 3))
      y[i] = rbinom(1, 1, turnout$c)
    }
  }
  x_meas <- colnames(probs)[max.col(probs)]
  x_meas <- gsub("pr_", "", x_meas)
  
  dat <- cbind(y, confound, x, x_meas, probs) |> 
    as_tibble()
  
  stan_list <- list(
    N = nrow(dat),
    K = length(unique(x)),
    x = as.numeric(as.factor(x)),
    x_meas = as.numeric(as.factor(x_meas)),
    y = y,
    p = probs
  )
  
  return(lst(dat, stan_list))
}


sim_binary_data <- function(N, turnout, bias) {
  race = sample(c("black", "white"),
                size = N,
                replace = TRUE)
  confound = rbinom(N, 1, 0.5)
  
  y = NULL
  probs = tibble(prob_black = NA, prob_white = NA)
  for (i in seq_len(N)) {
    if (race[i] == "black" & confound[i] == 1) {
      probs[i, ] = rdirichlet(1, c(1, 2))
      y[i] = rbinom(1, 1, turnout$black + bias)
    } else if (race[i] == "black" & confound[i] == 0) {
      probs[i, ] = rdirichlet(1, c(4, 1))
      y[i] = rbinom(1, 1, turnout$black)
    } else if (race[i] == "white" & confound[i] == 1) {
      probs[i, ] = rdirichlet(1, c(1, 4))
      y[i] = rbinom(1, 1, turnout$white + bias)
    } else if (race[i] == "white" & confound[i] == 0) {
      probs[i, ] = rdirichlet(1, c(1, 4))
      y[i] = rbinom(1, 1, turnout$white)
    }
  }
  race_meas <- colnames(probs)[max.col(probs)]
  race_meas <- gsub("prob_", "", race_meas)
  
  # Tibble data format
  dat <- cbind(y, confound, race, race_meas, probs) |> 
    as_tibble()
  
  # Stan data for true race
  stan_list <- list(
    N = N,
    K = length(unique(race)),
    x = as.numeric(as.factor(race)),
    x_meas = as.numeric(as.factor(race_meas)),
    y = y,
    p = cbind(probs$prob_black, probs$prob_white)
  )
  
  return(lst(dat, stan_list))
}


sim_cont_data <- function(N, noise_a, true_b) {
  

  # 
  # 
  # dat <- tibble(
  #   x = rnorm(N, sd = 4),
  #   activity = rpois(N, lambda = 1)
  # ) |> 
  #   mutate(x_sd = abs((abs(x) * (noise_x / 4)) +  
  #                       (activity * noise_a) +    
  #                       rnorm(n(), sd = .1)),     
  #          y = rnorm(n(), x * true_b),
  #          x_meas = rnorm(n(), x, x_sd))
  
  dat <- tibble(
    x = rnorm(N),
    activity = rpois(N, lambda = 1)
  ) |> 
    mutate(x_sd = abs(activity * noise_a + rnorm(n(), sd = .1)),
           y = rnorm(n(), x * true_b),
           x_meas = rnorm(n(), x, x_sd))
  
  corr_x <- cor(dat$x, dat$x_meas)
  
  stan_list <- list(
    N = N,
    x = dat$x,
    x_meas = dat$x_meas,
    x_sd = dat$x_sd,
    y = dat$y,
    skew = 0
  )
  
  return(lst(dat, stan_list, corr_x, true_b))
}


sim_bias_cont_data <- function(N, true_b = 0, bias = 0) {
  
  dat <- tibble(
    x = rnorm(N),
    u = rnorm(N)
  ) |> 
    mutate(y = rnorm(n(), x * true_b + u * bias),
           x_sd = runif(n(), .1, 1),
           x_meas = sn::rsn(n(), xi = x, omega = x_sd, alpha = u))
  
  stan_list <- list(
    N = N,
    x_meas = dat$x_meas,
    x_sd = dat$x_sd,
    x_skew = dat$u,
    y = dat$y,
    skew = 1
  )
  
  return(lst(dat, stan_list))
}
# d <- sim_bias_cont_data(1000, bias = 1)
# broom::tidy(lm(y ~ x, data = d))
# broom::tidy(lm(y ~ x_meas + u, data = d))


sim_bias_dist_data <- function(N, n_draws = 10000, true_b = 0, bias) {
  dat <- tibble(
    unit = as.character(1:N),
    x = rnorm(N),
    u = rnorm(N, sd = 1),
    x_sd = rexp(N)
  ) |>
    # filter(abs(u) < 1) |>
    crossing(draw = 1:n_draws) |>
    group_by(unit) |>
    mutate(x_dist = rsn(n(), xi = x, omega = 1, alpha = u),
           # x_dist = rnorm(n(), mean = x),
           x_meas = mean(x_dist)) |> 
    nest_by() |> 
    mutate(mod = list(selm(x_dist ~ 1, data = data)),
           dp = list(extractSECdistr(mod)),
           xi = slot(dp, "dp")[["xi"]],
           omega = slot(dp, "dp")[["omega"]],
           alpha = slot(dp, "dp")[["alpha"]]) |> 
    unnest(data) |> 
    dplyr::select(unit, x, u, x_meas, xi, omega, alpha) |> 
    distinct() |> 
    mutate(y = rnorm(n(), x * true_b + u * bias)) |> 
    ungroup()
  
  stan_list <- list(
    N = nrow(dat),
    x_meas = dat$x_meas,
    x_xi = dat$xi,
    x_sd = dat$omega,
    x_skew = dat$alpha,
    y = dat$y
  )
  
  return(lst(dat, stan_list))
}
# sim_bias_dist_data(10, bias = 10)$dat


# set.seed(111)
# x <- rst(1000, xi = 0, omega = 1, alpha = -0.5, nu = 30)
# mod <- selm(x ~ 1, family = "ST")
# extractSECdistr(mod)



sim_irt_data <- function(n_groups, n_items, missing_pct) {
  groups = tibble(group = as.character(1:n_groups),
                  partic_level = rbeta(n_groups, shape1 = 3, shape2 = 5),
                  theta_i = rnorm(n_groups))
  dat = tibble(item = as.character(1:n_items),
               beta_j = rnorm(n_items),
               type = sample(c("a", "b"), n_items, replace = TRUE)) |> 
    mutate(gamma_j = case_when(type == "a" ~ rnorm(n(), mean = -.5),
                               type == "b" ~ rnorm(n(), mean = .5))) |> 
    crossing(groups) |> 
    mutate(p_y = pnorm(gamma_j * theta_i + beta_j),
           y = rbinom(n(), 1, p_y),
           participate = rbinom(n(), 1, partic_level))
  
  stan_list = list(
    N = nrow(dat),
    J = n_groups,
    K = n_items,
    jj = 1:n_groups,
    kk = 1:n_items,
    y = dat$y
  )
  
  return(lst(dat, stan_list))
}


sim_skewt <- function(N, xi, omega, alpha) {
  x <- rsn(n = N, xi = xi, omega = omega, alpha = alpha)
  stan_list <- list(
    N = N,
    x = x
  )
  return(stan_list)
} 
