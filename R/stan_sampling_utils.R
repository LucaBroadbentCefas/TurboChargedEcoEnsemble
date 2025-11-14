stan_sampling_with_filter <- function(mod, data, control = NULL, ...) {
  warn_buf <- character(0)

  fit <- withCallingHandlers(
    rstan::sampling(mod, data = data, control = control, ...),
    warning = function(w) {
      warn_buf <<- c(warn_buf, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (length(warn_buf) > 0) {
    # preserve order, drop duplicates
    msgs <- warn_buf[!duplicated(warn_buf)]
    # robust match for Stan treedepth warnings
    is_treedepth <- grepl(
      "exceeded the maximum treedepth|maximum\\s+treedepth(.*exceeded)?",
      msgs, ignore.case = TRUE, perl = TRUE
    )
    is_pairs <- grepl(
      "examine\\s+the\\s+pairs\\(\\)\\s+plot",
      msgs, ignore.case = TRUE, perl = TRUE
    )

    suppressible <- is_treedepth | is_pairs

    if (!all(suppressible)) {
      # other warnings present -> print everything (including treedepth/pairs)
      for (m in msgs) warning(m, call. = FALSE, immediate. = TRUE)
    }
  }

  return(fit)
}
