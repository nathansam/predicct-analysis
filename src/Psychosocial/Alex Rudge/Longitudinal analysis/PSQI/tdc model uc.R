library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/PSQI/")

source("tdc data.R")
#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model
# UC
data_soft_merged_uc <- data_soft_merged %>% dplyr::filter(diagnosis2 == "UC/IBDU")
data_hard_merged_uc <- data_hard_merged %>% dplyr::filter(diagnosis2 == "UC/IBDU")

## Soft ####

cox_soft_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    SleepDisturbance +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_soft_merged_uc
)

cox_soft_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_hard_uc <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    SleepDisturbance +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_hard_merged_uc
)

cox_hard_uc %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
