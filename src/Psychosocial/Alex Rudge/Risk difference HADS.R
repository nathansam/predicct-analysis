library(tidyverse)
library(magrittr)
library(survival)
library(mice)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Risk difference
# Complete case

# Anxiety

summon_risk_difference_factor_boot(
  data = data_survival_anxiety_soft_uc,
  model = cox_anxiety_soft_uc,
  time = 365,
  variable = 'score_group'
) %>%
  dplyr::mutate(flare_type = "soft", diagnosis2 = "UC/IBDU")




summon_risk_difference_factor_boot_mice(
  data = data_survival_anxiety_soft_uc,
  model = cox_anxiety_soft_uc,
  time = 365,
  variable = 'score_group'
) 

