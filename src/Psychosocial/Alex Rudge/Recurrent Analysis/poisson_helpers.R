# Shared setup for the recurrent-event (Poisson/negative-binomial) analysis.
# Sourced by Poisson Analysis.qmd (and Events per
# individual.R's own demo). Model fitting itself is NOT wrapped in a
# function here - Poisson Analysis.qmd writes out each glmer()/glmer.nb()
# call explicitly per flare-type/diagnosis-group combination, the same way
# Primary Analysis writes out each coxph() call, rather than looping over
# them - only shared plumbing (outcome construction, glmer_control, model-
# checking plots) lives here.
#
# Provides: event_counts_soft, event_counts_hard (with covariates joined,
# Sex/age_decade/Smoke/IMD/FC/SiteNo/diagnosis2), glmer_control, and the
# model-checking plot functions.

library(patchwork)  # p1 + p2 in plot_model_checks()

# Assumes the working directory is this "Recurrent Analysis" folder (true by
# default when Quarto renders a .qmd from here) - adjust the path if sourcing
# from elsewhere.
source("build_event_counts.R")

# Bound Optimization BY Quadratic Approximationn (Nelder-Mead failed to converge; BOBYQA more estable for GLMMs)
glmer_control <- lme4::glmerControl(optimizer = "bobyqa")

plot_model_checks <- function(model, title) {
  # Tibble with fitted values and residuals
  resid_df <- tibble::tibble(fitted = fitted(model), resid = residuals(model, type = "pearson"))
  
  p1 <- ggplot2::ggplot(resid_df, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = paste(title, "- residuals vs fitted"), x = "Fitted", y = "Pearson residual") + ggplot2::theme_minimal()
  
  p2 <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::labs(title = "QQ plot", x = "Theoretical", y = "Sample") + ggplot2::theme_minimal()
  
  p1 + p2
}

# Predictive check
plot_ppc <- function(model, observed_n_events, title) {
  
  dplyr::bind_rows(
    tibble::tibble(n_events = observed_n_events, type = "Observed"),
    tibble::tibble(n_events = simulate(model, nsim = 1)[[1]], type = "Model-predicted")
  ) %>%
    dplyr::count(type, n_events) %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(n_events), y = prop, fill = type)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = title, x = "Number of events", y = "Proportion", fill = NULL) +
    ggplot2::theme_minimal()
}
