plot_model_predictions <- function(
    model, data, coefs, nsim = 500,
    mean_bout_m, sd_bout_m,
    mean_bout_f, sd_bout_f,
    custom_colors,
    ip_values = c(0.05, 0.5, 0.9),
    ylim = c(0, 30), xlim = c(0, 40),
    save_path = NULL,
    include_title = FALSE,
    model_label = NULL,
    auto_name = FALSE,
    clean_names = TRUE,
    fixed_only = TRUE,
    wrap_width = 55,
    facet = TRUE,
    facet_nrow = NULL,
    facet_ncol = NULL) {

  # Required packages
  require(data.table)
  require(ggplot2)
  require(foreach)

  # labels
  if (auto_name && is.null(model_label)) {
    model_label <- generate_model_label(model, clean_names = clean_names, 
    wrap_width = wrap_width)
  }

  # 1. Generate fixed effect predictions
  pred_fixed <- predict_fixed_effects_scaled_model(
    model = model,
    nsim = nsim,
    mean_bout_m = mean_bout_m,
    sd_bout_m = sd_bout_m,
    mean_bout_f = mean_bout_f,
    sd_bout_f = sd_bout_f,
    coefs = coefs,
    bout_m_range = range(data$bout_m),
    ip_values = ip_values
  )
  pred_fixed[, prop_ip := as.character(prop_ip)] # ensure prop_ip is a factor with clean labels

  # 2. Generate population predictions if fixed_only=FALSE
  if (!fixed_only) {
    lat_pops <- unique(data$lat_pop)
    pred_all_pops <- data.table::rbindlist(lapply(lat_pops, function(pop) {
      predict_conditional_scaled_mean(
        model = model,
        group_id = pop,
        group_var = "lat_pop",
        data = data,
        nsim = nsim,
        mean_bout_m = mean_bout_m,
        sd_bout_m = sd_bout_m,
        mean_bout_f = mean_bout_f,
        sd_bout_f = sd_bout_f,
        coefs = coefs,
        ip_values = ip_values
      )
    }))
    pred_all_pops[, prop_ip := as.character(prop_ip)]
  }

  # 3. Create the plot
  facet_labels <- c("0.05" = "early", "0.5" = "mid", "0.9" = "late")
  
  p <- ggplot(data = pred_fixed)
  if (!fixed_only) {
    p <- p + geom_line(data = pred_all_pops, aes(x = bout_m, y = pred, group = lat_pop),
                       color = 'grey50', alpha = 0.5, lwd = 0.25)
  }
  p <-  p +
    geom_ribbon(aes(x = bout_m, ymin = lwr, ymax = upr, fill = factor(prop_ip)), alpha = 0.3) +
    geom_line(aes(x = bout_m, y = pred, col = factor(prop_ip))) +
    scale_color_manual(values = custom_colors, labels = facet_labels, guide = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = custom_colors, labels = facet_labels, guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous("\u2642 bout [hours]") +
    scale_y_continuous("\u2640 bout [hours]", expand = c(0, 0)) +
    coord_cartesian(ylim = ylim, xlim = xlim) + 
    #facet_wrap(~prop_ip, labeller = as_labeller(facet_labels)) +
    labs_obj +
    theme_MB +
    theme(
      #legend.position = "none",
      legend.position = if (facet) "none" else "right",plot.title = element_text(hjust = 0, size = rel(0.75)),
      plot.subtitle = element_text(hjust = 0.5, size = rel(0.85), margin = margin(b = 1)),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1))
    )

  # Add facets or labels conditionally
  if (facet) {
    labs_obj <- if (include_title) {
          labs(subtitle = "Incubation period", title = model_label)
        } else {
          labs(subtitle = "Incubation period")
        }
     p <- p + facet_wrap(~prop_ip, labeller = as_labeller(facet_labels), nrow = facet_nrow, ncol = facet_ncol)
      p <- p + labs_obj 
  } else {
    labs_obj <- if (include_title) {
          labs(subtitle = "Incubation period", title = model_label, color = "Incubation period", fill = "Incubation period")
        } else {
          labs(subtitle = "Incubation period")
        }
    p <- p + labs_obj
  }

  # 4. Optionally save
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 7, height = 5, units = "cm")
  }

  return(p)
}