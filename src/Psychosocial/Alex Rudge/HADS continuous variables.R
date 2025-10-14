library(tidyverse)
library(magrittr)
library(survival)

# Treating age and FC as continuous variables

# Run HADS Baseline.qmd prior to the analysis to get the data
# Called hads_for_analysis, rename to data_baseline
data_baseline <- hads_for_analysis

# FC maximum detectable value is 1250
data_baseline %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
)

# Anxiety
data_baseline_anxiety <- data_baseline %>% 
  dplyr::filter(hads_type == 'anxiety_hads')

# Patient reported flares
data_survival_anxiety_soft <- hads_for_analysis %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)


# Baseline

# Age
data_baseline_anxiety %>%
  ggplot(aes(x = age, colour = score_group)) +
  geom_density()

# FC

# Log transform due to extreme positive skew
data_baseline_anxiety %>%
  ggplot(aes(x = log(FC), colour = score_group)) +
  geom_density()


# Visualise the continuous variables effects of survival

# Age 
cox_anxiety_soft_age <- coxph(
  Surv(time, DiseaseFlareYN) ~ pspline(age, df = 2),
  data = data_survival_anxiety_soft
)

# Predictions from a cox at certain times at certain ages
data_plot_survival_age <- tidyr::expand_grid(
  age = seq(from = 20, to = 80, by = 1),
  time = seq(from = 100, to = 600, by = 100)
  ) %>%
  dplyr::mutate(DiseaseFlareYN = 1)

# Predictions
cox_anxiety_soft_age_predict <- predict(
  cox_anxiety_soft_age, 
  type = 'expected',
  se = TRUE,
  newdata = data_plot_survival_age)

# Plot
data_plot_survival_age %>%
  dplyr::mutate(
    expected = cox_anxiety_soft_age_predict$fit,
    se = cox_anxiety_soft_age_predict$se.fit,
  ) %>% 
  dplyr::mutate(
    conf.low = expected - 1.96*se,
    conf.high = expected + 1.96*se
  ) %>%
  dplyr::mutate(
    survival = exp(-expected),
    conf.low = exp(-conf.low),
    conf.high = exp(-conf.high)
  ) %>%
  ggplot(aes(x = age, y = survival, ymin = conf.low, ymax = conf.high, colour = forcats::as_factor(time))) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(fill = forcats::as_factor(time)), alpha = 0.5)

# FC
cox_anxiety_soft_fc <- coxph(
  Surv(time, DiseaseFlareYN) ~ pspline(log(FC), df = 2),
  data = data_survival_anxiety_soft
)

# Predictions from a cox at certain times at certain ages
data_plot_survival_fc <- tidyr::expand_grid(
  FC = seq(from = 20, to = 1000, by = 25),
  time = seq(from = 100, to = 600, by = 100)
) %>%
  dplyr::mutate(DiseaseFlareYN = 1)

# Predictions
cox_anxiety_soft_fc_predict <- predict(
  cox_anxiety_soft_fc, 
  type = 'expected',
  se = TRUE,
  newdata = data_plot_survival_fc)

# Plot
data_plot_survival_fc %>%
  dplyr::mutate(
    expected = cox_anxiety_soft_fc_predict$fit,
    se = cox_anxiety_soft_fc_predict$se.fit,
  ) %>% 
  dplyr::mutate(
    conf.low = expected - 1.96*se,
    conf.high = expected + 1.96*se
  ) %>%
  dplyr::mutate(
    survival = exp(-expected),
    conf.low = exp(-conf.low),
    conf.high = exp(-conf.high)
  ) %>%
  ggplot(aes(x = FC, y = survival, ymin = conf.low, ymax = conf.high, colour = forcats::as_factor(time))) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(fill = forcats::as_factor(time)), alpha = 0.5)



# Survival analysis
cox_anxiety_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    Sex +
    pspline(age, df = 2) +
    pspline(log(FC), df = 2) +
    frailty(SiteNo),
  data = data_survival_anxiety_soft
)

cox_anxiety_soft %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

# Looking at the HR there is little to no difference in the effects of our primary exposures.