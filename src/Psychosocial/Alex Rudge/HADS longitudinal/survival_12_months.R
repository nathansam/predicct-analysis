library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/HADS longitudinal/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")


# Plotting Kaplan-Meier curves
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

legend.title = 'HADS Anxiety Score'
legend.labs = c('0-7', '8-21')
palette = okabe_ito
dependent = 'score_group'

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )


# Survival model with 12 months as the baseline
data_survival_anxiety_soft_12m <- data_survival_anxiety_soft %>%
  dplyr::mutate(score_group = cut(
    anxiety_hads_12,
    breaks = c(0, 7, 21),
    labels = c("0-7", "8-21"),
    include.lowest = TRUE
  )) %>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

data_survival_anxiety_hard_12m <- data_survival_anxiety_hard %>%
  dplyr::mutate(score_group = cut(
    anxiety_hads_12,
    breaks = c(0, 7, 21),
    labels = c("0-7", "8-21"),
    include.lowest = TRUE
  )) %>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

data_survival_depression_soft_12m <- data_survival_depression_soft %>%
  dplyr::mutate(score_group = cut(
    depression_hads_12,
    breaks = c(0, 7, 21),
    labels = c("0-7", "8-21"),
    include.lowest = TRUE
  )) %>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )

data_survival_depression_hard_12m <- data_survival_depression_hard %>%
  dplyr::mutate(score_group = cut(
    depression_hads_12,
    breaks = c(0, 7, 21),
    labels = c("0-7", "8-21"),
    include.lowest = TRUE
  )) %>%
  # Remove patients with an event before 12 months
  dplyr::filter(
    time > 365
  ) %>%
  # 12 months is now baseline
  dplyr::mutate(
    time = time - 365
  )


# Kaplan-meier
# Anxiety
summon_km_curves(
  data = data_survival_anxiety_soft_12m,
  dependent = dependent,
  title = "Time to patient-reported flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

summon_km_curves(
  data = data_survival_anxiety_hard_12m,
  dependent = dependent,
  title = "Time to objective flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

# Depression
summon_km_curves(
  data = data_survival_depression_soft_12m,
  dependent = dependent,
  title = "Time to patient-reported flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

summon_km_curves(
  data = data_survival_depression_hard_12m,
  dependent = dependent,
  title = "Time to objective flare in IBD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)
