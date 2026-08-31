# Recurrent event analysis - Primary Analysis (duplicated)
#
# Standalone demo: plots the distribution of flare events per individual and
# models counts (diagnosis2 only) via Poisson/negative-binomial regression.
# Run interactively, not sourced blind. Outcome-building logic now lives in
# build_event_counts.R (also used by poisson_helpers.R for the per-exposure
# analyses in HADS.qmd etc.) - see that file for the endpoint definitions and
# event_counts_soft/hard construction.
library(patchwork)  # combines model-check plots; lme4 used via :: below

source("build_event_counts.R")

# ---- 1. Plot the distribution -------------------------------------------------

plot_event_distribution <- function(event_counts, title) {
  event_counts %>%
    dplyr::count(n_events) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(n_events), y = n)) +
    ggplot2::geom_col(fill = "#0F8B8D") +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4, size = 3.2) +
    ggplot2::labs(title = title, x = "Number of flare events", y = "Number of participants") +
    ggplot2::theme_minimal()
}

plot_event_distribution(event_counts_soft, "Distribution of patient-reported flare episodes per individual")
plot_event_distribution(event_counts_hard, "Distribution of objective flare episodes per individual")

# Variance:mean >> 1 signals overdispersion (negative binomial > Poisson).
dispersion_summary <- function(n_events, label) {
  m <- mean(n_events); v <- var(n_events)
  cat(label, "- Mean:", round(m, 3), "| Variance:", round(v, 3), "| Ratio:", round(v / m, 2), "\n")
}
dispersion_summary(event_counts_soft$n_events, "Soft")
dispersion_summary(event_counts_hard$n_events, "Hard")

# ---- 2. Poisson/negative-binomial regression, with a random effect for site --
#
# (1|SiteNo) in every model, matching frailty(SiteNo) in this repo's Cox
# models. diagnosis2 is the only exposure available so far - extend the
# formula once real exposures (HADS, PHQ, etc.) are merged in. Requires
# lme4 (install if glmer()/glmer.nb() aren't found - not yet in renv.lock).
glmer_control <- lme4::glmerControl(optimizer = "bobyqa")

fit_flare_models <- function(data, adjusted) {
  f <- if (adjusted) {
    n_events ~ diagnosis2 + Sex + age_decade + Smoke + IMD + FC + (1 | SiteNo)
  } else {
    n_events ~ diagnosis2 + (1 | SiteNo)
  }
  list(
    poisson = lme4::glmer(f, data = data, family = poisson, control = glmer_control),
    nb = lme4::glmer.nb(f, data = data, control = glmer_control)
  )
}

model_specs <- list(
  `Soft, unadjusted` = list(data = event_counts_soft, adjusted = FALSE, model_label = "Unadjusted", outcome = "Patient-reported (soft)"),
  `Soft, adjusted`   = list(data = event_counts_soft, adjusted = TRUE,  model_label = "Adjusted",   outcome = "Patient-reported (soft)"),
  `Hard, unadjusted` = list(data = event_counts_hard, adjusted = FALSE, model_label = "Unadjusted", outcome = "Objective (hard)"),
  `Hard, adjusted`   = list(data = event_counts_hard, adjusted = TRUE,  model_label = "Adjusted",   outcome = "Objective (hard)")
) %>%
  purrr::map(~ modifyList(.x, list(fit = fit_flare_models(.x$data, .x$adjusted))))

summary(model_specs$`Soft, adjusted`$fit$poisson)
summary(model_specs$`Hard, adjusted`$fit$poisson)

# Much lower NB AIC confirms overdispersion (above) - report NB, not Poisson.
purrr::iwalk(model_specs, ~ cat(.y, "- Poisson AIC:", round(AIC(.x$fit$poisson), 1), "| NB AIC:", round(AIC(.x$fit$nb), 1), "\n"))

# ---- 3. Rate ratio forest plot -------------------------------------------------
#
# Uses the NB fit throughout - switch to $poisson above if the AIC
# comparison says otherwise for your data.
tidy_rate_ratios <- function(model, model_label, outcome_label) {
  est <- lme4::fixef(model)
  se <- sqrt(diag(as.matrix(vcov(model))))
  tibble::tibble(term = names(est), estimate = est, std.error = se) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      rate_ratio = exp(estimate), lower = exp(estimate - 1.96 * std.error), upper = exp(estimate + 1.96 * std.error),
      model = model_label, outcome = outcome_label
    )
}

rate_ratios <- purrr::map_dfr(model_specs, ~ tidy_rate_ratios(.x$fit$nb, .x$model_label, .x$outcome))

rate_ratios %>%
  ggplot2::ggplot(ggplot2::aes(x = rate_ratio, y = term, colour = model)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = lower, xmax = upper), position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~outcome) +
  ggplot2::labs(x = "Rate ratio (95% CI, log scale)", y = NULL, colour = "Model") +
  ggplot2::theme_minimal()

# ---- 4. Model checking (visual) ------------------------------------------------
#
# Residuals-vs-fitted + QQ plot, and a posterior-predictive check (simulate
# from the fitted model, compare to the observed count distribution).
plot_model_checks <- function(model, title) {
  resid_df <- tibble::tibble(fitted = fitted(model), resid = residuals(model, type = "pearson"))
  p1 <- ggplot2::ggplot(resid_df, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = paste(title, "- residuals vs fitted"), x = "Fitted", y = "Pearson residual") + ggplot2::theme_minimal()
  p2 <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::labs(title = "QQ plot", x = "Theoretical", y = "Sample") + ggplot2::theme_minimal()
  p1 + p2
}

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

adjusted_specs <- purrr::keep(model_specs, ~ .x$adjusted)
purrr::iwalk(adjusted_specs, ~ print(plot_model_checks(.x$fit$nb, paste0(.y, " (NB)"))))
purrr::iwalk(adjusted_specs, ~ print(plot_ppc(.x$fit$nb, .x$data$n_events, paste0(.y, ": observed vs model-predicted"))))
