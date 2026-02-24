library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("tdc data.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model

# Anxiety ####
## Soft ####

cox_anxiety_soft <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_soft_merged
)

cox_anxiety_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_anxiety_hard <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_hard_merged
)

cox_anxiety_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# Depression ####
## Soft ####

cox_depression_soft <- coxph(
  Surv(tstart, tstop, endpoint) ~
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_soft_merged
)

cox_depression_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_depression_hard <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_hard_merged
)

cox_depression_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
