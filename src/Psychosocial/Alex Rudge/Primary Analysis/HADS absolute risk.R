library(tidyverse)
library(survival)
library(tictoc)


# Functions
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Run HADS

# Anxiety

# Soft
# UC
tic()
absolute_risk_anxiety_soft_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_anxiety_soft_uc,
  model = cox_anxiety_soft_uc,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'anxiety',
    flare_type = 'soft',
    diagnosis = 'UC'
  )
toc()


# CD
absolute_risk_anxiety_soft_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_anxiety_soft_cd,
  model = cox_anxiety_soft_cd,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'anxiety',
    flare_type = 'soft',
    diagnosis = 'CD'
  )

# Hard
# UC
absolute_risk_anxiety_hard_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_anxiety_hard_uc,
  model = cox_anxiety_hard_uc,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'anxiety',
    flare_type = 'hard',
    diagnosis = 'UC'
  )

# CD
absolute_risk_anxiety_hard_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_anxiety_hard_cd,
  model = cox_anxiety_hard_cd,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'anxiety',
    flare_type = 'hard',
    diagnosis = 'CD'
  )

# Combine
absolute_risk_anxiety_bootstraps <- dplyr::bind_rows(
  absolute_risk_anxiety_soft_uc,
  absolute_risk_anxiety_soft_cd,
  absolute_risk_anxiety_hard_uc,
  absolute_risk_anxiety_hard_cd
)

# Summarise the bootstrapping
absolute_risk_anxiety <- absolute_risk_anxiety_bootstraps %>%
  dplyr::group_by(flare_type, diagnosis, time, variable, level) %>%
  # Bootstrapped estimate and confidence intervals
  dplyr::summarise(
    estimate = mean(absolute_risk, na.rm = TRUE),
    conf.low = quantile(absolute_risk, prob = 0.025, na.rm = TRUE),
    conf.high = quantile(absolute_risk, prob = 0.975, na.rm = TRUE),
    nboot = sum(!is.na(absolute_risk))
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Partially adjusted/"

readr::write_rds(
  x = absolute_risk_anxiety_bootstraps,
  file = paste0(filepath, "absolute_risk_anxiety_bootstraps.rds")
)

readr::write_rds(
  x = absolute_risk_anxiety,
  file = paste0(filepath, "absolute_risk_anxiety.rds")
)

# Depression

# Soft
# UC
absolute_risk_depression_soft_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_depression_soft_uc,
  model = cox_depression_soft_uc,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'depression',
    flare_type = 'soft',
    diagnosis = 'UC'
  )

# CD
absolute_risk_depression_soft_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_depression_soft_cd,
  model = cox_depression_soft_cd,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'depression',
    flare_type = 'soft',
    diagnosis = 'CD'
  )

# Hard
# UC
absolute_risk_depression_hard_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_depression_hard_uc,
  model = cox_depression_hard_uc,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'depression',
    flare_type = 'hard',
    diagnosis = 'UC'
  )

# CD
absolute_risk_depression_hard_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_depression_hard_cd,
  model = cox_depression_hard_cd,
  time = 730,
  variable = 'score_group',
  nboot = 1000
) %>%
  dplyr::mutate(
    variable = 'depression',
    flare_type = 'hard',
    diagnosis = 'CD'
  )

# Combine
absolute_risk_depression_bootstraps <- dplyr::bind_rows(
  absolute_risk_depression_soft_uc,
  absolute_risk_depression_soft_cd,
  absolute_risk_depression_hard_uc,
  absolute_risk_depression_hard_cd
)

# Summarise the bootstrapping
absolute_risk_depression <- absolute_risk_depression_bootstraps %>%
  dplyr::group_by(flare_type, diagnosis, time, variable, level) %>%
  # Bootstrapped estimate and confidence intervals
  dplyr::summarise(
    estimate = mean(absolute_risk, na.rm = TRUE),
    conf.low = quantile(absolute_risk, prob = 0.025, na.rm = TRUE),
    conf.high = quantile(absolute_risk, prob = 0.975, na.rm = TRUE),
    nboot = sum(!is.na(absolute_risk))
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Partially adjusted/"

readr::write_rds(
  x = absolute_risk_depression_bootstraps,
  file = paste0(filepath, "absolute_risk_depression_bootstraps.rds")
)

readr::write_rds(
  x = absolute_risk_depression,
  file = paste0(filepath, "absolute_risk_depression.rds")
)
