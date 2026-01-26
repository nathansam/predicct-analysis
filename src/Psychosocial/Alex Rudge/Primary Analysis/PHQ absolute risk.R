library(tidyverse)
library(survival)
library(tictoc)


# Functions
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Run PHQ

# Soft
# UC
tic()
absolute_risk_soft_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_soft_uc,
  model = cox_soft_uc,
  time = 730,
  variable = 'somatisation',
  nboot = 1000
) %>%
  dplyr::mutate(
    flare_type = 'soft',
    diagnosis = 'UC'
  )
toc()


# CD
absolute_risk_soft_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_soft_cd,
  model = cox_soft_cd,
  time = 730,
  variable = 'somatisation',
  nboot = 1000
) %>%
  dplyr::mutate(
    flare_type = 'soft',
    diagnosis = 'CD'
  )

# Hard
# UC
absolute_risk_hard_uc <- summon_absolute_risk_factor_boot(
  data = data_survival_hard_uc,
  model = cox_hard_uc,
  time = 730,
  variable = 'somatisation',
  nboot = 1000
) %>%
  dplyr::mutate(
    flare_type = 'hard',
    diagnosis = 'UC'
  )

# CD
absolute_risk_hard_cd <- summon_absolute_risk_factor_boot(
  data = data_survival_hard_cd,
  model = cox_hard_cd,
  time = 730,
  variable = 'somatisation',
  nboot = 1000
) %>%
  dplyr::mutate(
    flare_type = 'hard',
    diagnosis = 'CD'
  )

# Combine
absolute_risk_bootstraps <- dplyr::bind_rows(
  absolute_risk_soft_uc,
  absolute_risk_soft_cd,
  absolute_risk_hard_uc,
  absolute_risk_hard_cd
)

# Summarise the bootstrapping
absolute_risk <- absolute_risk_bootstraps %>%
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
  x = absolute_risk_bootstraps,
  file = paste0(filepath, "absolute_risk_phq_bootstraps.rds")
)

readr::write_rds(
  x = absolute_risk,
  file = paste0(filepath, "absolute_risk_phq.rds")
)