
plot_multiple_models_combined(
  model_list = models,
  data_list = rep(list(u), length(models)),
  coefs_list = rep(list(coefs), length(models)),
  custom_colors = custom_colors,
  mean_bout_m = mean(u$bout_m),
  sd_bout_m = sd(u$bout_m),
  mean_bout_f = mean(u$bout_f),
  sd_bout_f = sd(u$bout_f),
  auto_name = TRUE,  # ← uses the cleaned model formula to label plots
  clean_names = TRUE
)


# OLD
# function to plot multiple models and combine vertically
plot_multiple_models_combined <- function(model_list, data_list, coefs_list, labels, custom_colors,
                                          mean_bout_m, sd_bout_m, mean_bout_f, sd_bout_f,
                                          nsim = 500, save_path = NULL) {
  require(patchwork)

  plots <- Map(function(mod, dat, coefs, label) {
    plot_model_predictions(
      model = mod,
      data = dat,
      coefs = coefs,
      nsim = nsim,
      mean_bout_m = mean_bout_m,
      sd_bout_m = sd_bout_m,
      mean_bout_f = mean_bout_f,
      sd_bout_f = sd_bout_f,
      custom_colors = custom_colors,
      model_label = label
    )
  }, model_list, data_list, coefs_list, labels)

  combined_plot <- wrap_plots(plots, ncol = 1)

  if (!is.null(save_path)) {
    ggsave(save_path, combined_plot, width = 12, height = 5 * length(plots), units = "cm")
  }

  return(combined_plot)
}


# 1. Fit all your models
m_1 = lmer(bout_f_z ~ 
                bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
                (1|genus) + (1|species) + 
                (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
                data = u)

m_1_s = lmer(bout_f_z ~ 
                bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
                (1|genus) +
                (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
                data = u)

m_2 = lmer(bout_f_z ~ 
                bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
                (1|genus) + (1|species) + 
                (bout_m_z + bout_pol1 + bout_pol2 | lat_pop),
                data = u)


m_3 = lmer(bout_f_z ~ 
                bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
                (1|genus) + (1|species) + 
                (bout_pol1 + bout_pol2 | lat_pop),
                data = u)

m_4 = lmer(bout_f_z ~ 
                bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
                (1|genus) + (1|species) + 
                (bout_m_z| lat_pop),
                data = u)

# 2. Prepare the inputs
model_list <- list(m_1, m_2, m_3, m_4)
data_list <- rep(list(u), length(model_list) )
coefs_list <- rep(list(coefs), length(model_list) )
labels <- c(
  "Random slopes:  ♂ bout × quadratic incubation period",
  "♂ bout + poly(incubation period)",
  "Quadratic incubation period",
  "♂ bout"
)
# 3. Generate the combined plot
combined_plot <- plot_multiple_models_combined(
  model_list = model_list,
  data_list = data_list,
  coefs_list = coefs_list,
  labels = labels,
  custom_colors = custom_colors,
  mean_bout_m = mean(u$bout_m),
  sd_bout_m = sd(u$bout_m),
  mean_bout_f = mean(u$bout_f),
  sd_bout_f = sd(u$bout_f),
  nsim = 500,
  save_path = "Output/Fig_S_within-combined.png"
)

