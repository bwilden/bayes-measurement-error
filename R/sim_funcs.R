

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
    N = N,
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


sim_cont_data <- function(N, noise_x, noise_a, true_b, bias = 0) {
  dat <- tibble(
    x = rnorm(N, sd = 4),
    activity = rpois(N, lambda = 1)
  ) |> 
    mutate(x_sd = abs((abs(x) * (noise_x / 4)) +  # extremism error
                        (activity * noise_a) +    # participation error
                        rnorm(n(), sd = .1)),     # extra random noise
           x_meas = rnorm(n(), x + bias, x_sd),
           y = rnorm(n(), x * true_b + activity * bias))
  
  corr_x <- cor(dat$x, dat$x_meas)
  
  stan_list <- list(
    N = N,
    x = dat$x,
    x_meas = dat$x_meas,
    x_sd = dat$x_sd,
    y = dat$y
  )
  
  return(lst(dat, stan_list, corr_x, true_b))
}
# a<-sim_cont_data(10000)
# 
# ggplot(a$dat) +
#   aes(x = x, y = x_meas) +
#   geom_point()
# 
# summary(lm(y ~ abs(x), data = a))
# summary(lm(y ~ abs(x_meas), data = a))

# library(rstan)
# library(MCMCpack)
# 
# N = 200
# x = sample(c("a", "b", "c"), N, replace = TRUE)
# mu_a = 0.7
# mu_b = 0.5
# mu_c = 0.3
# 
# y = NULL
# probs = data.frame(pr_a = NA, pr_b = NA, pr_c = NA)
# for (i in seq_len(N)) {
#   if (x[i] == "a") {
#     probs[i, ] = rdirichlet(1, c(3, 1, 1))
#     y[i] = rbinom(1, 1, mu_a)
#   } else if (x[i] == "b") {
#     probs[i, ] = rdirichlet(1, c(1, 3, 1))
#     y[i] = rbinom(1, 1, mu_b)
#   } else if (x[i] == "c") {
#     probs[i, ] = rdirichlet(1, c(1, 1, 3))
#     y[i] = rbinom(1, 1, mu_c)
#   }
# }
# x_meas <- colnames(probs)[max.col(probs)]
# x_meas <- gsub("pr_", "", x_meas)
# 
# stan_list <- list(
#   N = N,
#   K = length(unique(x)),
#   x = as.numeric(as.factor(x)),
#   x_meas = as.numeric(as.factor(x_meas)),
#   y = y,
#   pr_X = probs
# )
# 
# model <- "
# data {
#   int<lower=1> N; 
#   int<lower=1> K;  // number of categories
#   vector[N] y;
#   array[N, K] real<lower=0,upper=1> pr_X;
# }
# 
# parameters {
#   vector[K] mu;
#   real<lower=0> sigma;
# }
# 
# model {
#   mu ~ normal(0, 2);
#   sigma ~ student_t(3, 0, 2);
# 
#   array[K] real likelihood_terms;
#   for(i in 1:N){
#     for(j in 1:K){
#       likelihood_terms[j] = log(pr_X[i, j]) + normal_lpdf(y[i] | mu[j], sigma);
#     }
#     target += log_sum_exp(likelihood_terms);
#   }
# }
# "
# 
# fit <- stan(model_code = model, data = stan_list, iter = 2000, chains = 4)

