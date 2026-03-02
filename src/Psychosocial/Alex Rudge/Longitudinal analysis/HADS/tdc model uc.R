library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("tdc data.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model
# UC
data_anxiety_soft_merged_uc <- data_anxiety_soft_merged %<>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_anxiety_hard_merged_uc <- data_anxiety_hard_merged %>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_depression_soft_merged_uc <- data_depression_soft_merged %>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_depression_hard_merged_uc <- data_depression_hard_merged %>% dplyr::filter(diagnosis2 == 'UC/IBDU')

# Anxiety ####
## Soft ####

cox_anxiety_soft_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_soft_merged_uc
)

cox_anxiety_soft_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_anxiety_hard_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_hard_merged_uc
)

cox_anxiety_hard_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# Depression ####
## Soft ####

cox_depression_soft_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_soft_merged_uc
)

cox_depression_soft_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_depression_hard_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_hard_merged_uc
)

cox_depression_hard_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
