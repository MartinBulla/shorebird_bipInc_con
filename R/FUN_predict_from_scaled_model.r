predict_fixed_effects_scaled_model <- function(
      model, nsim = 500, 
      mean_bout_m, sd_bout_m, 
      mean_bout_f, sd_bout_f, 
      coefs, bout_m_range, 
      ip_values = c(0.05, 0.5, 0.9)) {
  require(arm)
  require(data.table)
  require(foreach)
  
  bsim <- sim(model, n.sim = nsim)
  v <- apply(bsim@fixef, 2, quantile, prob = 0.5)
  
  newD <- foreach(i = ip_values, .combine = rbind) %do% {
    data.table(bout_m = seq(bout_m_range[1], bout_m_range[2], length.out = 100), prop_ip = i)
  }
  
  new_poly <- poly(newD$prop_ip, degree = 2, coefs = coefs)
  newD[, bout_pol1 := new_poly[, 1]]
  newD[, bout_pol2 := new_poly[, 2]]
  newD[, bout_m_z := (bout_m - mean_bout_m) / sd_bout_m]
  
  X <- model.matrix(~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2, newD)
  
  predmatrix <- matrix(nrow = nrow(newD), ncol = nsim)
  for (j in 1:nsim) {
    predmatrix[, j] <- X %*% bsim@fixef[j, ]
  }
  
  newD[, pred_z := X %*% v]
  newD[, lwr_z := apply(predmatrix, 1, quantile, prob = 0.025)]
  newD[, upr_z := apply(predmatrix, 1, quantile, prob = 0.975)]
  
  newD[, pred := pred_z * sd_bout_f + mean_bout_f]
  newD[, lwr := lwr_z * sd_bout_f + mean_bout_f]
  newD[, upr := upr_z * sd_bout_f + mean_bout_f]
  
  return(newD)
}

predict_conditional_scaled_model_mean <- function(model, group_id, group_var = "lat_pop",
                                            data,  # full dataset
                                            nsim = 500,  
                                            mean_bout_m, sd_bout_m,
                                            mean_bout_f, sd_bout_f,
                                            coefs,
                                            ip_values = c(0.05, 0.5, 0.9)) {
  # Filter for group-specific data
  pop_data <- data[get(group_var) == group_id]
  if (nrow(pop_data) == 0) stop(paste("No data for group:", group_id))

  # Determine range of bout_m for this population
  bout_m_range <- range(pop_data$bout_m, na.rm = TRUE)

  # Simulate posterior draws
  sim_result <- arm::sim(model, n.sim = nsim)
  beta_fix <- colMeans(sim_result@fixef)

  # Random effects for the group
  re_draw <- sim_result@ranef[[group_var]]
  group_index <- which(dimnames(re_draw)[[2]] == group_id)

  if (length(group_index) == 0) stop("Group ID not found in random effects.")

  # Mean random effects across draws
  re_j <- colMeans(re_draw[, group_index, ])
  names(re_j) <- dimnames(re_draw)[[3]]

  # Align random effects with fixed effect names
  coef_names <- names(lme4::fixef(model))
  re_aligned <- setNames(rep(0, length(coef_names)), coef_names)
  re_aligned[names(re_j)] <- re_j

  beta_combined <- beta_fix + re_aligned

  # Generate prediction grid
  newD <- foreach::foreach(i = ip_values, .combine = rbind) %do% {
    data.table::data.table(
      bout_m = seq(bout_m_range[1], bout_m_range[2], length.out = 100),
      prop_ip = i
    )
  }

  # Apply orthogonal poly transform
  new_poly <- poly(newD$prop_ip, degree = 2, coefs = coefs)
  newD[, bout_pol1 := new_poly[, 1]]
  newD[, bout_pol2 := new_poly[, 2]]
  newD[, bout_m_z := (bout_m - mean_bout_m) / sd_bout_m]

  # Create model matrix and predict
  X <- model.matrix(~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2, data = newD)
  newD[, pred_z := as.vector(X %*% beta_combined)]
  newD[, pred := pred_z * sd_bout_f + mean_bout_f]
  newD[, (group_var) := group_id]

  return(newD)
}

predict_conditional_scaled_model <- function(model, group_id, group_var = "lat_pop", nsim = 1000,
                                             mean_bout_m, sd_bout_m, 
                                             mean_bout_f, sd_bout_f, 
                                             coefs, bout_m_range, 
                                             ip_values = c(0.05, 0.5, 0.9)) {
  # Simulate from the full posterior
  bsim <- arm::sim(model, n.sim = nsim)
  
  # Generate prediction grid
  newD <- foreach::foreach(i = ip_values, .combine = rbind) %do% {
    data.table::data.table(
      bout_m = seq(bout_m_range[1], bout_m_range[2], length.out = 100),
      prop_ip = i
    )
  }
  
  # Apply polynomial transformation
  new_poly <- poly(newD$prop_ip, degree = 2, coefs = coefs)
  newD[, bout_pol1 := new_poly[, 1]]
  newD[, bout_pol2 := new_poly[, 2]]
  
  # Scale bout_m to match model input
  newD[, bout_m_z := (bout_m - mean_bout_m) / sd_bout_m]
  
  # Build model matrix
  X <- model.matrix(~ bout_m_z * bout_pol1 + bout_m_z * bout_pol2, data = newD)
  
  # Get coefficient names (from fixed effects)
  coef_names <- names(lme4::fixef(model))
  
  # Initialize prediction matrix
  predmatrix <- matrix(nrow = nrow(newD), ncol = nsim)
  
  # Loop over posterior draws
  for (j in 1:nsim) {
    
    # Get fixed effects for this draw
    beta_fix <- bsim@fixef[j, ]
    
    # Get random effects for group, or fallback to 0
    re_draw <- bsim@ranef[[group_var]]
    
    if (!(group_id %in% dimnames(re_draw)[[2]])) {
      stop(paste("Group ID", group_id, "not found in model's random effects."))
    }
    
    group_index <- which(dimnames(re_draw)[[2]] == group_id)
    re_j <- bsim@ranef[[group_var]][j, group_index, ]
    names(re_j) <- names(fixef(model))  # ensure names for matching

    # Align random effects with fixed effect names
    re_aligned <- setNames(rep(0, length(coef_names)), coef_names)
    re_aligned[names(re_j)] <- re_j
    
    # Total effect: fixed + random
    beta_j <- beta_fix + re_aligned
    
    # Predict: X %*% beta
    predmatrix[, j] <- X %*% beta_j
  }
  
  # Median and 95% CI in scaled space
  newD[, pred_z := apply(predmatrix, 1, median)]
  newD[, lwr_z := apply(predmatrix, 1, quantile, prob = 0.025)]
  newD[, upr_z := apply(predmatrix, 1, quantile, prob = 0.975)]
  
  # Back-transform to original outcome scale
  newD[, pred := pred_z * sd_bout_f + mean_bout_f]
  newD[, lwr := lwr_z * sd_bout_f + mean_bout_f]
  newD[, upr := upr_z * sd_bout_f + mean_bout_f]
  
  # Add group label
  newD[, (group_var) := group_id]
  
  return(newD)
}