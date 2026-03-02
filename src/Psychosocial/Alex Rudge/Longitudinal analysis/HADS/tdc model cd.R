library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("tdc data.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Time dependent cox model
# CD
data_anxiety_soft_merged_cd <- data_anxiety_soft_merged %<>% dplyr::filter(diagnosis2 == 'CD')
data_anxiety_hard_merged_cd <- data_anxiety_hard_merged %>% dplyr::filter(diagnosis2 == 'CD')
data_depression_soft_merged_cd <- data_depression_soft_merged %>% dplyr::filter(diagnosis2 == 'CD')
data_depression_hard_merged_cd <- data_depression_hard_merged %>% dplyr::filter(diagnosis2 == 'CD')

# Anxiety ####
## Soft ####

cox_anxiety_soft_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_soft_merged_cd
)

cox_anxiety_soft_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_anxiety_hard_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_hard_merged_cd
)

cox_anxiety_hard_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# Depression ####
## Soft ####

cox_depression_soft_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_soft_merged_cd
)

cox_depression_soft_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_depression_hard_cd <- coxph(
  Surv(tstart, tstop, endpoint) ~ 
    score_group +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_hard_merged_cd
)

cox_depression_hard_cd %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
