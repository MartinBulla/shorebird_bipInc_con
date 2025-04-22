# EXPLORE influence of random structure

## functions
fit_model_variants <- function(base_formula, random_structures, data, labels = NULL) {
  models <- lapply(random_structures, function(rnd) {
    lmer(as.formula(paste0(base_formula, rnd)), data = data)
  })

  if (!is.null(labels)) {
    names(models) <- labels
  }

  return(models)
}

source('R/FUN_predict_from_scaled_model.r')
source('R/FUN_generate_model_label.r')
source('R/FUN_plot_model_predictions.R')
source('R/FUN_plot_multiple_models_combined_2.r')


## create the list of models for plotting
base_formula <- "bout_f_z ~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + (1|genus) + (1|species)"

random_structures <- c(
  " + (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop)",
  " + (bout_m_z + bout_pol1 + bout_pol2 | lat_pop)",
  " + (bout_pol1 + bout_pol2 | lat_pop)",
  " + (bout_m_z | lat_pop)"
)

models <- fit_model_variants(base_formula, random_structures, data = u)

# plot multiple models and combine vertically
custom_colors <- pal_locuszoom()(7)[4:2]

plot_multiple_models_combined(
  model_list = models,
  save_path = "Output/Fig_E_clean-T_v2.png",
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

plot_multiple_models_combined(
  model_list = models,
  save_path = "Output/Fig_E_clean-false_v3.png",
  data_list = rep(list(u), length(models)),
  coefs_list = rep(list(coefs), length(models)),
  custom_colors = custom_colors,
  mean_bout_m = mean(u$bout_m),
  sd_bout_m = sd(u$bout_m),
  mean_bout_f = mean(u$bout_f),
  sd_bout_f = sd(u$bout_f),
  auto_name = TRUE,  # ← uses the cleaned model formula to label plots
  clean_names = FALSE,
  facet = FALSE, width =8, height = 24
)

random_structures <- c(
  " + (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop)",
  " + (bout_m_z + bout_pol1 + bout_pol2 | lat_pop)",
  " + (bout_pol1 + bout_pol2 | lat_pop)",
  " + (bout_m_z | lat_pop)"
)


# SPLINES
u[, ns_prop := ns(prop_ip, df = 3) ]
# Create spline basis (this returns a matrix with df columns)
spline_mat <- ns(u$prop_ip, df = 3)
model_spline <- lmer(
  bout_f_z ~ bout_m_z * (ns_prop1 + ns_prop2 + ns_prop3) +
    (1 | genus) + (1 | species) + (bout_m_z | lat_pop),
  data = u
)
scatter.smooth(fitted(model_spline),sqrt(abs(resid(model_spline))), col='grey')

# Assign each column into the data.table
u[, paste0("ns_prop", 1:3) := as.data.table(spline_mat)]

# nlme - model var - NO CHANGE
library(nlme)  

## Add fitted values
model = mbi5zp
u$fitted_vals <- fitted(model)

## Create quartile-based bins
u$fitted_class <- cut(u$fitted_vals,
                         breaks = quantile(u$fitted_vals, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = c("Q1", "Q2", "Q3", "Q4"))
## deciles bines
u$fitted_class_2 <- cut(u$fitted_vals,
                         breaks = quantile(u$fitted_vals, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = paste0("D", 1:10))                        
## Fixed-width bins (if you want even ranges, not even counts)
u$fitted_class_3 <- cut(u$fitted_vals, breaks = seq(min(u$fitted_vals), max(u$fitted_vals), length.out = 5),
                         include.lowest = TRUE)
lme_model <- lme( 
      bout_f_z ~ bout_m_z * poly(prop_ip, 2), 
      random = ~ 1 | lat_pop,  # or species, one level only! 
      weights = varIdent(form = ~1 | fitted_class_2), 
      data = u,
      control = lmeControl(maxIter = 500, msMaxIter = 500, opt = "optim")
    )  

scatter.smooth(fitted(lme_model),sqrt(abs(resid(lme_model))), col='grey')

## Fit a simple model first
start_model <- lme(
  bout_f_z ~ bout_m_z * poly(prop_ip, 2),
  random = ~ 1 | lat_pop,
  data = u
)

# Now fit with varPower, using starting values from above
lme_model <- update(
  start_model,
  weights = varPower(form = ~ fitted(.)),
  control = lmeControl(maxIter = 500, msMaxIter = 500)
)

scatter.smooth(fitted(lme_model),sqrt(abs(resid(lme_model))), col='grey')

ggplot(u, aes(x = bout_f)) + geom_density() + scale_x_continuous(trans='log10')

m_log <- lmer(scale(log(bout_f)) ~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
              (1|genus) + (1|species) + 
              (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
              data = u)

scatter.smooth(fitted(m_log),sqrt(abs(resid(m_log))), col='grey') # better

m_log_both <- lmer(scale(log(bout_f)) ~ scale(log(bout_m)) * bout_pol1 + scale(log(bout_m)) * bout_pol2 + 
              (1|genus) + (1|species) + 
              (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
              data = u)

scatter.smooth(fitted(m_log_both),sqrt(abs(resid(m_log_both))), col='grey') 

m_sqrt <- lmer(scale(sqrt(bout_f)) ~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2 + 
              (1|genus) + (1|species) + 
              (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
              data = u)

scatter.smooth(fitted(m_sqrt),sqrt(abs(resid(m_sqrt))), col='grey') # worse

###

check_model_batch(
  model_list = models,
  data_list = rep(list(u), length(model_list)),
  model_names = c("Table SXA", "Table SXB", "Table SXC", "Table SXD"),
  cont_list = rep(list(c('bout_m', 'prop_ip')), length(model_list)),
  trans_list = rep(list(c('none', 'none')), length(model_list)),
  spatial = TRUE,
  temporal = TRUE,
  PNG = TRUE,
  n_col = 6, n_row = 3
)

Got it, then is there a way to extract the dataset from the model_list? Surely the model formulate contains the defined dataset behind data = ""

m_ass('test', mbi5zp, dat = u, fixed = c('bout_m_z', 'prop_ip'),  trans = c('none', 'none'))

plot_model_predictions(
  model = mbi5zp,
  data = u,
  coefs = coefs,
  nsim = 500,
  mean_bout_m = mean(u$bout_m),
  sd_bout_m = sd(u$bout_m),
  mean_bout_f = mean(u$bout_f),
  sd_bout_f = sd(u$bout_f),
  custom_colors = custom_colors,
  save_path = "Output/Fig_S_w2_correct-poly_v3-test_v2.png",
  facet = FALSE,
  width = 12, height = 20
)



labels = c( 
      "Random slopes:  ♂ bout × quadratic incubation period", 
      "Random slopes:♂ bout + quadratic incubation period", 
      "Random slopes: Quadratic incubation period", 
      "Random slopes: ♂ bout" 
    ),


names(model_list) =  




model <- lmer(bout_f_z ~ bout_m_z * poly(prop_ip, 2) + (1 | genus) + (1 | species) + (bout_m_z | lat_pop), data = u)

# Bootstrap vs sim comparison
library(lme4)
library(boot)
library(arm)
library(data.table)

## model
model =  lmer(bout_f_z ~ 
            bout_m_z * bout_pol1 + 
            bout_m_z * bout_pol2 + 
            (1|genus) + (1|species) + 
            (bout_m_z * bout_pol1 + bout_m_z * bout_pol2 | lat_pop),
            data = u)

## bootMer() for fixed effects
boot_fx <- bootMer(
  model,
  FUN = fixef,   # You could also extract predictions, R², etc.
  nsim = 1000,
  type = "parametric",  # or "residual"
  seed = 123
)

boot_fx_dt <- as.data.table(boot_fx$t)
setnames(boot_fx_dt, names(fixef(model)))
boot_fx_dt[, method := "bootMer"]

## sim() for fixed effects
sim_fx <- sim(model, n.sims = 1000)
sim_fx_dt <- as.data.table(sim_fx@fixef)
sim_fx_dt[, method := "sim"]

## combine fixed effects and plot
fx_combined <- rbind(boot_fx_dt, sim_fx_dt)
fx_long <- data.table(melt(fx_combined, id.vars = "method", variable.name = "term", value.name = "estimate"))

ggplot(fx_long , aes(x = term, y = estimate, fill = method)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(0.2), shape = 21, size = 2) +
  theme_minimal() + coord_flip()+
  facet_wrap(~term, scales = "free", ncol = 1) +
  labs(title = "Fixed Effects: bootMer vs sim", y = "Estimate", x = NULL) + 
  theme(
    strip.text = element_blank(),     # Removes the text in the facet label
    strip.background = element_blank()  # Optional: removes the background of the facet label
  )

  ggplot(fx_long  , aes(x = term, y = estimate, fill = method)) +
  geom_violin(alpha = 0.7)+#, position = position_dodge(0.2)) +
  #stat_summary(fun = mean, geom = "point")+#, position = position_dodge(0.2), shape = 21, size = 2) +
  theme_minimal() + #coord_flip()+
  labs(title = "Fixed Effects: bootMer vs sim", y = "Estimate", x = NULL)


# plotting random effects
library(data.table)

# Get random effects and conditional variances
re_list <- ranef(model, condVar = TRUE)

# Combine all into one long table
re_dt <- rbindlist(
  lapply(names(re_list), function(grouping) {
    ran <- re_list[[grouping]]
    postvar <- attr(ran, "postVar")

    group_names <- rownames(ran)
    n_groups <- nrow(ran)
    n_terms <- ncol(ran)

    # Turn RE values into data.table
    re_vals <- as.data.table(ran)
    re_vals[, group := group_names]
    re_vals[, grouping_factor := grouping]

    # Extract SEs safely
    if (n_terms == 1) {
      se_mat <- data.table(se = sqrt(postvar[1, 1, ]))
      setnames(se_mat, "se", names(re_vals)[1])
    } else {
      se_mat <- matrix(NA_real_, nrow = n_groups, ncol = n_terms)
      for (i in 1:n_groups) {
        se_mat[i, ] <- sqrt(diag(postvar[, , i]))
      }
      se_mat <- as.data.table(se_mat)
      setnames(se_mat, names(re_vals)[1:n_terms])
    }

    se_mat[, group := group_names]
    se_mat[, grouping_factor := grouping]

    # Melt both for long format
    re_long <- melt(re_vals, id.vars = c("group", "grouping_factor"),
                    variable.name = "term", value.name = "estimate")
    se_long <- melt(se_mat, id.vars = c("group", "grouping_factor"),
                    variable.name = "term", value.name = "se")

    # Merge by all keys
    merged <- merge(re_long, se_long, by = c("group", "grouping_factor", "term"))
    return(merged)
  }),
  use.names = TRUE
)



ggplot(re_dt, aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  facet_grid(term ~ grouping_factor, scales = "free", switch = "y") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random effects with conditional 95% intervals",
       x = NULL, y = "BLUP estimate") +
  theme(strip.text.y = element_text(angle = 0))

re_dt[, facet_label := paste0(grouping_factor, ": ", term)]

ggplot(re_dt, aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  facet_wrap(~facet_label, scales = "free_y", ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(title = "Random effects with conditional 95% intervals",
       x = NULL, y = "BLUP estimate") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 6))

re_dt[, effect_type := ifelse(term == "(Intercept)", "Intercept", "Slope")]
ggplot(re_dt, aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  facet_grid(term ~ effect_type + grouping_factor, scales = "free_y", switch = "y") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random effects with conditional 95% intervals",
       x = NULL, y = "BLUP estimate") +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_text(size = 6))

re_dt[, effect_type := ifelse(term == "(Intercept)", "Intercept", "Slope")]
re_dt[, slope_label := ifelse(effect_type == "Intercept", NA, paste0(grouping_factor, ": ", term))]

# A. Plot Intercepts Separately
p_intercepts <- ggplot(re_dt[effect_type == "Intercept"],
       aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  facet_grid(term ~ grouping_factor, scales = "free", switch = "y") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Intercepts with conditional 95% intervals",
       x = NULL, y = "BLUP estimate") +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_text(size = 6))


# B. Plot Slopes Separately
p_slopes <- ggplot(re_dt[effect_type == "Slope"],
       aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.2) +
  facet_wrap(~slope_label, scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Slopes with conditional 95% intervals",
       x = NULL, y = "BLUP estimate") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 6))


p_intercepts / p_slopes + plot_layout(heights = c(1, 2))

p_intercepts |   p_slopes


ggplot(re_dt[effect_type == "Intercept"],
       aes(x = reorder(group, estimate), y = estimate)) +
  geom_point(size = 1.3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se,
                    ymax = estimate + 1.96 * se),
                width = 0.2) +
  facet_wrap(~ grouping_factor, scales = "free", ncol = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Intercepts (by grouping factor)",
       x = NULL, y = "BLUP estimate") +
  geom_hline(yintercept = 0, linetype = "dashed") 
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 6))


# Get bootstrap CIs
boot.ci(boot_res, type = "perc", index = 1)  # for intercept
boot.ci(boot_res, type = "perc", index = 2)  # for bout_m_z, etc.


# bootstrap random

# 1. Let boot_sd run overnight:
boot_sd <- bootMer(model, 
  FUN = function(x) {
    unlist(lapply(VarCorr(x), function(vc) attr(vc, "stddev")))
  },
  nsim = 1000,
  seed = 123
)
save(boot_sd, file = 'Data/boot_res_full_random.RData')
boot_sd_dt <- as.data.table(boot_sd$t)
boot_sd_dt[, method := "bootMer"]

# 2. Simulate from sim() for REs
library(arm)

sim_re <- sim(model, n.sims = 1000)

sim_sd_long <- rbindlist(
  lapply(names(sim_re@ranef), function(grouping) {
    re_array <- sim_re@ranef[[grouping]]  # 3D: [group, term, sim]
    term_names <- dimnames(re_array)[[2]]
    n_sims <- dim(re_array)[3]

    # For each term
    rbindlist(lapply(term_names, function(term) {
      # Pull matrix [group, sim]
      values <- re_array[, term, ]
      if (is.null(dim(values))) {
        values <- matrix(values, nrow = 1)  # only 1 group level
      }

      # SD across group levels, one per sim
      sds <- apply(values, 2, sd)

      data.table(
        component = paste0(grouping, ":", term),
        sd = sds
      )
    }))
  }),
  use.names = TRUE
)

sim_sd_long[, method := "sim"]


# 3. plot
re_combined <- rbind(boot_sd_dt, sim_sd_long, fill = TRUE)
re_long <- melt(re_combined, id.vars = "method", variable.name = "component", value.name = "sd")

re_long_clean <- re_long[
  re_long[, .N, by = .(component, method)][N > 1],
  on = .(component, method)
]


ggplot(re_long_clean, aes(x = component, y = sd, fill = method)) +
  geom_violin(alpha = 0.4, position = position_dodge(0.6)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, position = position_dodge(0.6)) +
  theme_minimal() + coord_flip()+
  labs(title = "Random Effect SDs: bootMer vs sim", y = "Standard deviation", x = NULL)

ggplot(re_long_clean, aes(x = component, y = sd, fill = method)) +
  geom_violin(data = re_long_clean[, .N, by = .(component, method)][N > 1][
    re_long_clean, on = .(component, method)],
    alpha = 0.4, position = position_dodge(0.6)) +
  geom_point(data = re_long_clean[, .N, by = .(component, method)][N == 1][
    re_long_clean, on = .(component, method)],
    shape = 21, size = 2, position = position_dodge(0.6)) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, position = position_dodge(0.6)) +
  theme_minimal() +
  coord_flip()


setDT(re_long)
re_long[!is.finite(sd), .N, by = .(component, method)][order(-N)]  
