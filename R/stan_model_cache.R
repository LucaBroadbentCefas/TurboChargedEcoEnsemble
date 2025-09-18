# Utilities for loading Stan programs at runtime.

# cache environment for explicit-sampling Stan models
.stan_model_cache_explicit <- new.env(parent = emptyenv())

get_explicit_stanmodel <- function(model_name) {
  if (!nzchar(model_name)) {
    stop("`model_name` must be a non-empty string.", call. = FALSE)
  }
  if (!exists(model_name, envir = .stan_model_cache_explicit, inherits = FALSE)) {
    stan_file <- system.file("stan", paste0(model_name, ".stan"), package = "EcoEnsemble")
    if (!nzchar(stan_file)) {
      stan_file <- file.path("inst", "stan", paste0(model_name, ".stan"))
    }
    if (!file.exists(stan_file)) {
      stop(sprintf("Stan file for model '%s' not found.", model_name), call. = FALSE)
    }
    assign(
      model_name,
      rstan::stan_model(file = stan_file, allow_undefined = TRUE),
      envir = .stan_model_cache_explicit
    )
  }
  get(model_name, envir = .stan_model_cache_explicit, inherits = FALSE)
}
