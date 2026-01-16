library(tidyverse)
library(magrittr)
library(survival)
library(mice)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions risk difference mice.R")

# Risk difference
# Complete case

nboot = 99

# Anxiety

summon_risk_difference_factor_boot(
  data = data_survival_anxiety_soft_uc,
  model = cox_anxiety_soft_uc,
  time = 365,
  variable = 'score_group',
  nboot = nboot
) %>%
  dplyr::mutate(flare_type = "soft", diagnosis2 = "UC/IBDU") %>%
  dplyr::bind_rows(
    summon_risk_difference_factor_boot(
      data = data_survival_anxiety_soft_cd,
      model = cox_anxiety_soft_cd,
      time = 365,
      variable = 'score_group',
      nboot = nboot
    ) %>%
      dplyr::mutate(flare_type = "soft", diagnosis2 = "CD")
  ) %>%
  dplyr::bind_rows(
    summon_risk_difference_factor_boot(
      data = data_survival_anxiety_hard_uc,
      model = cox_anxiety_hard_uc,
      time = 365,
      variable = 'score_group',
      nboot = nboot
    ) %>%
      dplyr::mutate(flare_type = "hard", diagnosis2 = "UC/IBDU")
  ) %>%
  dplyr::bind_rows(
    summon_risk_difference_factor_boot(
      data = data_survival_anxiety_hard_cd,
      model = cox_anxiety_hard_cd,
      time = 365,
      variable = 'score_group',
      nboot = nboot
    ) %>%
      dplyr::mutate(flare_type = "hard", diagnosis2 = "CD")
  )






summon_risk_difference_factor_boot_mice(
  data = data_survival_anxiety_soft_uc,
  model = cox_anxiety_soft_uc,
  time = 365,
  variable = 'score_group',
  m = 5,
  printFlag = FALSE,
  method = 'pmm'
) %>%
  dplyr::mutate(flare_type = "soft", diagnosis2 = "UC/IBDU")

