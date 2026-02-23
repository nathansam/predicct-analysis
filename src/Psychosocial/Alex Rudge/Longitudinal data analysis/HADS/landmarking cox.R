library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal data analysis/HADS/")

#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")
source("landmarking data.R")

# Landmark at 12 months.

# Anxiety ####
## Soft ####

cox_anxiety_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group_locf +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_soft
)

cox_anxiety_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_anxiety_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group_locf +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_anxiety_hard
)

cox_anxiety_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Depression ####
## Soft ####

cox_depression_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group_locf +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_soft
)

cox_depression_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard ####

cox_depression_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group_locf +
    IMD +
    Sex +
    age_decade +
    FC +
    Smoke +
    frailty(SiteNo),
  data = data_depression_hard
)

cox_depression_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
