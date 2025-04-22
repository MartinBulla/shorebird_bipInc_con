fit_model_variants <- function(base_formula, random_structures, data, labels = NULL) {
  models <- lapply(random_structures, function(rnd) {
    lmer(as.formula(paste0(base_formula, rnd)), data = data)
  })

  if (!is.null(labels)) {
    names(models) <- labels
  }

  return(models)
}