plot_multiple_models_combined <- function(
    model_list, data_list, coefs_list, labels = NULL, custom_colors,
    mean_bout_m, sd_bout_m, mean_bout_f, sd_bout_f,
    nsim = 500, save_path = NULL, fixed_only = TRUE,
    auto_name = FALSE, clean_names = TRUE, wrap_width = 55,# clean_names used only if auto_name = TRUE
    facet = TRUE, facet_nrow = NULL, facet_ncol = NULL,
    panel_nrow = NULL, panel_ncol = 1,
    axis_title_x = "♂ bout [hours]",
    axis_title_y = "♀ bout [hours]",
    width = 8, height = 20
    ) {
        
  require(patchwork)

  if (is.null(labels)) {
    if (auto_name) {
      labels <- sapply(model_list, generate_model_label,
                       clean_names = clean_names, wrap_width = wrap_width)
    } else {
      labels <- paste0("Model ", seq_along(model_list))
    }
  }
  
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
      model_label = label,
      include_title = TRUE,
      fixed_only = fixed_only,
      facet = facet,
      facet_nrow = facet_nrow,
      facet_ncol = facet_ncol,
      wrap_width = wrap_width
    )
  }, model_list, data_list, coefs_list, labels)

  #combined_plot <- wrap_plots(plots, ncol = 1)

  if (facet) {
    combined_plot <- wrap_plots(plots, ncol = panel_ncol, nrow = panel_nrow)
  } else {
    combined_plot <- wrap_plots(plots, ncol = panel_ncol, nrow = panel_nrow) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(
          legend.position = "bottom")
  }

  if (!is.null(save_path)) {
    ggsave(save_path, combined_plot, width = width, height = height, units = "cm")
  }

  return(combined_plot)
}
