library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Anxiety
# Soft
data <- data_anxiety_soft %>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Anxiety change binary positive or negative
  dplyr::mutate(
    anxiety_change_sign = sign(anxiety_change),
    anxiety_change_sign = dplyr::case_match(
      anxiety_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse",
      NA ~ "Missing"
    )
  ) %>%
  # Remove patients with event before 12m
  dplyr::filter(
    time > 365
  ) %>%
  # 12m is new baseline
  dplyr::mutate(
    time = time - 365
  ) %>%
  # Anxiety score at 12m 
  dplyr::mutate(
    score_group = cut(
      anxiety_hads_12,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(score_group)) %>%
  dplyr::mutate(
    anxiety_change_sign = forcats::fct_relevel(anxiety_change_sign,
                                               "Same",
                                               "Worse",
                                               "Better")
  )

# Cox model
model <- coxph(
  Surv(time, DiseaseFlareYN) ~ 
    score_group*anxiety_change_sign, 
  data = data)

model %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

summon_lrt(model, remove = 'anxiety_change_sign:score_group')

# Hard
data <- data_anxiety_hard %>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Anxiety change binary positive or negative
  dplyr::mutate(
    anxiety_change_sign = sign(anxiety_change),
    anxiety_change_sign = dplyr::case_match(
      anxiety_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse",
      NA ~ "Missing"
    ),
  ) %>%
  # Remove patients with event before 12m
  dplyr::filter(
    time > 365
  ) %>%
  # 12m is new baseline
  dplyr::mutate(
    time = time - 365
  ) %>%
  # Anxiety score at 12m 
  dplyr::mutate(
    score_group = cut(
      anxiety_hads_12,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(score_group)) %>%
  dplyr::mutate(
    anxiety_change_sign = forcats::fct_relevel(anxiety_change_sign,
                                               "Same",
                                               "Worse",
                                               "Better")
  )

# Cox model
model <- coxph(
  Surv(time, DiseaseFlareYN) ~ 
    score_group*anxiety_change_sign, 
  data = data)

model %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

summon_lrt(model, remove = 'anxiety_change_sign:score_group')


# Depression
# Soft
data <- data_depression_soft %>%
  # Calculate change
  dplyr::mutate(
    depression_change = depression_hads_12 - depression_hads
  ) %>%
  # depression change binary positive or negative
  dplyr::mutate(
    depression_change_sign = sign(depression_change),
    depression_change_sign = dplyr::case_match(
      depression_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse",
      NA ~ "Missing"
    )
  ) %>%
  # Remove patients with event before 12m
  dplyr::filter(
    time > 365
  ) %>%
  # 12m is new baseline
  dplyr::mutate(
    time = time - 365
  ) %>%
  # depression score at 12m 
  dplyr::mutate(
    score_group = cut(
      depression_hads_12,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(score_group)) %>%
  dplyr::mutate(
    depression_change_sign = forcats::fct_relevel(depression_change_sign,
                                               "Same",
                                               "Worse",
                                               "Better")
  )

# Cox model
model <- coxph(
  Surv(time, DiseaseFlareYN) ~ 
    score_group*depression_change_sign, 
  data = data)

model %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

summon_lrt(model, remove = 'depression_change_sign:score_group')

# Hard
data <- data_depression_hard %>%
  # Calculate change
  dplyr::mutate(
    depression_change = depression_hads_12 - depression_hads
  ) %>%
  # depression change binary positive or negative
  dplyr::mutate(
    depression_change_sign = sign(depression_change),
    depression_change_sign = dplyr::case_match(
      depression_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse",
      NA ~ "Missing"
    )
  ) %>%
  # Remove patients with event before 12m
  dplyr::filter(
    time > 365
  ) %>%
  # 12m is new baseline
  dplyr::mutate(
    time = time - 365
  ) %>%
  # depression score at 12m 
  dplyr::mutate(
    score_group = cut(
      depression_hads_12,
      breaks = c(0, 7, 21),
      labels = c("0-7", "8-21"),
      include.lowest = TRUE
    )
  ) %>%
  dplyr::filter(!is.na(score_group)) %>%
  dplyr::mutate(
    depression_change_sign = forcats::fct_relevel(depression_change_sign,
                                                  "Same",
                                                  "Worse",
                                                  "Better")
  )

# Cox model
model <- coxph(
  Surv(time, DiseaseFlareYN) ~ 
    score_group*depression_change_sign, 
  data = data)

model %>%
  broom::tidy(conf.int = TRUE, exp = TRUE)

summon_lrt(model, remove = 'depression_change_sign:score_group')
