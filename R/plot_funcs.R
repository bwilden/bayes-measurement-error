

# ME Coef Plots -----------------------------------------------------------

compare_cat_coefs <- function(me, meas) {
  fit_draws <- rbind(
    me |> mutate(model = "Measurement Error"),
    meas |> mutate(model = "Observed")
  ) |> 
    pivot_longer(cols = contains("beta"))
  
  p <- fit_draws |>
    ggplot(aes(x = value, y = model, group = name, fill = name)) +
    stat_slabinterval() +
    labs(y = "")  +
    geom_vline(xintercept = 0, 
               linetype = "dashed", 
               color = "grey",
               size = 1.25) +
    scale_fill_met_d("Hokusai2") +
    theme_ggdist()
  return(p)
}

compare_cont_coefs <- function(me, meas, true) {
  coefs <- rbind(me, meas)
  
  p <- coefs |> 
    mutate(model_name = case_when(model_name == "Mean Values Only" ~ "Mean Only Model",
                                  model_name == "Measurement Error" ~ "Measurement Error Model")) |> 
    ggplot(aes(x = corr_x, y = mean, color = model_name)) +
    geom_smooth(n = 8, fill = "gray", level = 0.89) +
    geom_pointrange(aes(ymin = `5.5%`, ymax = `94.5%`), size = .3) +
    geom_hline(yintercept = true, linetype = "dashed") +
    # geomtextpath::geom_texthline(label = paste("True Parameter Value =", true), 
    #                              yintercept = true, family = "serif") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_ggdist() +
    scale_color_met_d("Isfahan1") +
    scale_x_reverse(limits = c(1, 0)) +
    # scale_fill_met_d("Morgenstern") +
    theme(text = element_text(family = "serif"),
          legend.position = "top") +
    labs(y = "Parameter Estimate", x = "Correlation Between True Ideal Point and\nMean Measurement Error Ideal Point",
         color = "Model Type", caption = paste("True Coefficient Value =", true))
  return(p)
}

compare_cont_coefs2 <- function(me, meas, true) {
  fit_draws <- rbind(
    data.table::rbindlist(me) |> dplyr::select(beta, fit_name) |>
      mutate(model = "Measurement\nError"),
    data.table::rbindlist(meas) |> dplyr::select(beta, fit_name) |>
      mutate(model = "Observed")
  )
  p <- fit_draws |> 
    ggplot(aes(x = beta, y = model, fill = fit_name)) +
    stat_halfeye() +
    geom_vline(xintercept = true, 
               linetype = "dashed", 
               size = 1) +
    scale_fill_brewer(palette = "Reds") +
    labs(y = "", x = "Estimate", caption = paste("True Parameter Value =", true)) +
    theme_ggdist() +
    theme(text = element_text(family = "serif"))
  return(p)
}


# IRT Plots ---------------------------------------------------------------

create_theta_check_plot <- function(irt_df, irt_qis) {
  true_thetas <- irt_df |> 
    dplyr::select("group", "theta_i", "type") |> 
    distinct()
  
  groups <- left_join(irt_qis, true_thetas, by = "group")
  
  p <- groups |> 
    ggplot(aes(x = theta_i, y = mean, color = type)) +
    geom_pointrange(aes(ymin = q5, ymax = q95)) +
    geom_abline(slope = 1, intercept = 0)
  return(p)
}

create_theta_dist_plot <- function(irt_qis) {
  p <- irt_qis |> 
    arrange(mean) |> 
    rownames_to_column() |> 
    mutate(rowname = as.numeric(rowname)) |> 
    ggplot(aes(x = mean, y = rowname)) +
    geom_pointrange(aes(xmin = mean - sd, xmax = mean + sd), 
                    alpha = .75, size = .5, color = met.brewer("Isfahan1", 1)) +
    geom_pointrange(aes(xmin = mean - sd * 1.96, xmax = mean + sd * 1.96), 
                    alpha = .5, size = .5, color = met.brewer("Isfahan1", 1)) +
    labs(x = expression(paste("Estimated Ideal Point, ", theta^{`*`})), y = "Groups (sorted)",
         caption = "Means and Standard Errors", title = "Simulated IRT Results") +
    theme_ggdist() +
    theme(text = element_text(family = "serif"))
  
  return(p)
}
# create_theta_dist_plot(irt_qis = irt_qis)

compare_coefs <- function(me_draws, no_me_draws, true) {
  draws <- rbind(
    me_draws |> 
      dplyr::select("beta") |> 
      mutate(model = "Measurement Error\nModel"),
    no_me_draws |> 
      dplyr::select("beta") |> 
      mutate(model = "Mean Only Model")
  )
  
  p <- draws |>
    ggplot(aes(x = beta, y = model, fill = model)) +
    stat_slabinterval(alpha = .75, size = 4, fatten_point = 2) +
    labs(y = "")  +
    geom_vline(xintercept = true, 
               linetype = "dashed", 
               color = "grey",
               size = 1.25) +
    scale_fill_manual(values = met.brewer("Isfahan1", 2)) +
    labs(x = "Coefficient Estimate", caption = paste("True Coefficient Value =", true)) +
    theme_ggdist() +
    theme(text = element_text(family = "serif"),
          legend.position = "none")
  
  return(p)
}


# House Reelection Plots --------------------------------------------------

compare_reelection_coefs <- function(draws) {
  p <- draws |>
    ggplot(aes(x = beta1, y = model, fill = model)) +
    stat_slabinterval(alpha = .75, size = 2, fatten_point = 2,
                      .width = .89) +
    labs(x = "Election Share %", y = "",
         caption = "Independent variable is measured such that lower values correspond to more liberal,\nand higher values correspond to more conservative") +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = "grey",
               size = 1) +
    facet_wrap(~ party, nrow = 2) +
    scale_fill_manual(values = met.brewer("Isfahan1", 3)) +
    theme_ggdist() +
    theme(text = element_text(family = "serif"),
          legend.position = "none")
  
  return(p)
}
