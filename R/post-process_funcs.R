

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
    ggplot(aes(x = corr_x, y = mean, color = model_name)) +
    geom_smooth(n = 8, fill = "gray", level = 0.89) +
    # geom_pointrange(aes(ymin = `5.5%`, ymax = `94.5%`)) +
    # geom_hline(yintercept = true) +
    geomtextpath::geom_texthline(label = paste("True Parameter Value =", true), 
                                 yintercept = true, family = "serif") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_ggdist() +
    scale_color_met_d("Isfahan1") +
    scale_x_reverse(limits = c(1, 0)) +
    # scale_fill_met_d("Morgenstern") +
    theme(text = element_text(family = "serif"),
          legend.position = "top") +
    labs(y = "Parameter Estimate", x = "Correlation Between True Ideal Point and Mean Measurement Error Ideal Point",
         color = "Model Type")
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

# tar_load(me_cont_draws_me_cont)
# tar_load(true_cont_draws_no_me_cont)
# tar_load(obs_cont_draws_no_me_cont)
# 
# compare_cont_coefs(me_cont_draws_me_cont,
#                    true_cont_draws_no_me_cont,
#                    obs_cont_draws_no_me_cont)
