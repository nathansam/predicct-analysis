library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/HADS longitudinal/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Change in HADS between 0 and 12m and flare in next 12m

# Anxiety ####
## Soft ####
data_anxiety_soft %<>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Remove patients with an event or censored before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )
  

# Histogram of change in HADS
data_anxiety_soft %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  ggplot(aes(x = anxiety_change, group = DiseaseFlareYN, colour = DiseaseFlareYN)) +
  geom_density()


# Cox model
cox_anxiety_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    anxiety_change +
    frailty(SiteNo),
  data = data_anxiety_soft
)


# Test for a spline
summon_lrt(cox_anxiety_soft, remove = 'anxiety_change', add = 'ns(anxiety_change, df = 2)')


cox_anxiety_soft %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

## Hard flare ####

data_anxiety_hard %<>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Remove patients with an event or censored before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )


# Histogram of change in HADS
data_anxiety_hard %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  ggplot(aes(x = anxiety_change, group = DiseaseFlareYN, colour = DiseaseFlareYN)) +
  geom_density()


# Cox model
cox_anxiety_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    anxiety_change +
    frailty(SiteNo),
  data = data_anxiety_hard
)


# Test for a spline
summon_lrt(cox_anxiety_hard, remove = 'anxiety_change', add = 'ns(anxiety_change, df = 2)')

cox_anxiety_hard %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)
