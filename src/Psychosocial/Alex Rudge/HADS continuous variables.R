library(tidyverse)
library(magrittr)
library(survival)
library(splines)

# Treating age and FC as continuous variables

# Run HADS.qmd prior to the analysis to get the data
# Called hads_for_analysis, rename to data_baseline
data_baseline <- hads_for_analysis

# Age in decades
data_baseline %<>%
  dplyr::mutate(
    age_decade = age/10
  )


# FC maximum detectable value is 1250
data_baseline %<>%
  dplyr::mutate(
    FC = dplyr::case_when(
      FC > 1250 ~ 1250,
      .default = FC
    )
  )

# Log transform FC due to extreme positive skew
data_baseline %<>%
  dplyr::mutate(
    FC = log(FC)
  )

# Anxiety
data_baseline_anxiety <- data_baseline %>% 
  dplyr::filter(hads_type == 'anxiety_hads')

# Patient reported flares
data_survival_anxiety_soft <- data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)


# Baseline
data_baseline_anxiety %>%
  ggplot(aes(x = age_decade, colour = score_group)) +
  geom_density()

data_baseline_anxiety %>%
  ggplot(aes(x = FC, colour = score_group)) +
  geom_density()

# Visualise the continuous variables effects of survival
# Age 
cox_anxiety_soft_age <- coxph(
  Surv(time, DiseaseFlareYN) ~ age_decade,
  data = data_survival_anxiety_soft
)

# Do we need a spline?
summon_lrt(cox_anxiety_soft_age, remove = 'age_decade', add = 'ns(age_decade, df = 2)')
# No

plot_continuous_hr(
  data = data_survival_anxiety_soft,
  model = cox_anxiety_soft_age,
  variable = 'age_decade')

# FC
cox_anxiety_soft_fc <- coxph(
  Surv(time, DiseaseFlareYN) ~ FC,
  data = data_survival_anxiety_soft
)

# Do we need a spline?
summon_lrt(cox_anxiety_soft_fc, remove = 'FC', add = 'ns(FC, df = 2)')
# No

plot_continuous_hr(
  data = data_survival_anxiety_soft,
  model = cox_anxiety_soft_fc,
  variable = 'FC')


# MICE
data_survival_anxiety_soft %<>%
  dplyr::select(time, DiseaseFlareYN, score_group, Sex, age_decade, FC, SiteNo) %>%
  dplyr::mutate(cumhaz = mice::nelsonaalen(data = ., timevar = time, statusvar = DiseaseFlareYN))

# Predictor matrix - need to exclude time from the model
pred_matrix <- mice::make.predictorMatrix(data_survival_anxiety_soft)

pred_matrix[, 'time'] <- 0
pred_matrix[, 'SiteNo'] <- 0

data_survival_anxiety_soft_imputed <- data_survival_anxiety_soft %>% 
  mice::mice(m = 5, predictorMatrix = pred_matrix)

# Completed data
data_survival_anxiety_soft_all <- mice::complete(data_survival_anxiety_soft_imputed, action = 'all')


cox_anxiety_soft_results <- with(data_survival_anxiety_soft_imputed, coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    Sex +
    age_decade +
    FC +
    frailty(SiteNo))) %>%
  mice::pool() %>%
  summary(
    conf.int = TRUE,
    conf.level = 0.95,
    exponentiate = TRUE)

cox_anxiety_soft_results
