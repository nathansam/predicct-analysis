library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Exercise/")

source("tdc data.R")
#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model
# CD
data_soft_merged_cd <- data_soft_merged %>% dplyr::filter(diagnosis2 == "CD")
data_hard_merged_cd <- data_hard_merged %>% dplyr::filter(diagnosis2 == "CD")

## Soft ####

cox_soft_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    MinimumExercise +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_soft_merged_cd
)

cox_soft_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_hard_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    MinimumExercise +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_hard_merged_cd
)

cox_hard_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
