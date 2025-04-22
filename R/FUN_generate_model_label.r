generate_model_label <- function(model, clean_names = TRUE, wrap_width = 55) {
  wrap_formula_text <- function(text, width = wrap_width) {
    wrapped <- strwrap(text, width = width)
    paste(wrapped, collapse = "\n")
  }
  
  fixed <- tryCatch({
    rhs <- lme4::nobars(formula(model)[[3]])
    paste(deparse(rhs), collapse = "")
  }, error = function(e) "")

  random <- tryCatch({
    re <- lme4::findbars(formula(model))
    if (length(re)) paste(sapply(re, deparse), collapse = " + ") else ""
  }, error = function(e) "")

  if (clean_names) {
    fixed <- gsub("\\+ \\([^|]+\\|[^)]+\\)", "", fixed)
    fixed <- gsub("\\(1\\|[^)]+\\)", "", fixed)
    fixed <- gsub("\\s+", " ", fixed)
    fixed <- trimws(fixed)
    random <- trimws(random)
  }

  fixed <- wrap_formula_text(fixed)
  random <- wrap_formula_text(random)

  paste0("Fixed effects: ", fixed, "\nRandom effects: ", random)
}