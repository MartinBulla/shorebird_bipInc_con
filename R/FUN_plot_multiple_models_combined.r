plot_multiple_models_combined <- function(
            model_list, data_list, coefs_list, labels = NULL, custom_colors,
            mean_bout_m, sd_bout_m, mean_bout_f, sd_bout_f,
            nsim = 500, save_path = NULL, fixed_only = TRUE,
            auto_name = FALSE, clean_names = TRUE) {
                # clean_names used only if auto_name = TRUE
  
  require(patchwork)

  wrap_formula_text <- function(text, width = 55) {
    wrapped <- strwrap(text, width = width)
    paste(wrapped, collapse = "\n")
  }

  if (auto_name && is.null(labels)) {
    labels <- sapply(model_list, function(mod) {
      if (inherits(mod, "merMod")) {
        f <- deparse(formula(mod))
        full_formula <- paste(f, collapse = "")
         fixed <- tryCatch({
                full_rhs <- lme4::nobars(formula(mod)[[3]])
                paste(deparse(full_rhs), collapse = "")
                }, error = function(e) "")
        random <- tryCatch({
            re <- lme4::findbars(formula(mod))
            if (length(re)) paste(sapply(re, deparse), collapse = " + ") else ""
            }, error = function(e) "")
        if (clean_names) {
          fixed <- gsub("\\+ \\([^|]+\\|[^)]+\\)", "", fixed)
          fixed <- gsub("\\(1\\|[^)]+\\)", "", fixed)
          fixed <- gsub("\\s+", " ", fixed)
          fixed <- trimws(fixed)
          random <- gsub(".*~", "", random)
          random <- trimws(random)
        }
        fixed <- wrap_formula_text(fixed)
        random <- wrap_formula_text(random)
        paste0("Fixed effects: ", fixed, "\nRandom effects: ", random)
      } else {
        "Model"
      }
    })
  } else if (is.null(labels)) {
    labels <- paste0("Model ", seq_along(model_list))
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
      fixed_only = fixed_only
    )
  }, model_list, data_list, coefs_list, labels)

  combined_plot <- wrap_plots(plots, ncol = 1)

  if (!is.null(save_path)) {
    ggsave(save_path, combined_plot, width = 12, height = 5 * length(plots), units = "cm")
  }

  return(combined_plot)
}
