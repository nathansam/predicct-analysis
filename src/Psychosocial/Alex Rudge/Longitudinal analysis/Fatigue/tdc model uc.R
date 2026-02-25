library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

source("tdc data.R")
#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model

# UC
data_soft_merged %<>% dplyr::filter(diagnosis2 == "UC/IBDU")
data_hard_merged %<>% dplyr::filter(diagnosis2 == "UC/IBDU")

## Soft ####

cox_soft <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    OftenLackEnergy +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_soft_merged
)

cox_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_hard <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    OftenLackEnergy +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_hard_merged
)

cox_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
