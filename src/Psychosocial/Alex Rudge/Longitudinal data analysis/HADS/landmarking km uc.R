library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal data analysis/HADS/")

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")
source("landmarking data.R")

# UC data

data_anxiety_soft_uc <- data_anxiety_soft %>% dplyr::filter(diagnosis2 == "UC/IBDU")
data_anxiety_hard_uc <- data_anxiety_hard %>% dplyr::filter(diagnosis2 == "UC/IBDU")
data_depression_soft_uc <- data_depression_soft %>% dplyr::filter(diagnosis2 == "UC/IBDU")
data_depression_hard_uc <- data_depression_hard %>% dplyr::filter(diagnosis2 == "UC/IBDU")

# Landmark at 12 months.

# Plotting Kaplan-Meier curves
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

legend.title = 'HADS Anxiety Score'
legend.labs = c('0-7', '8-21')
palette = okabe_ito
dependent = 'score_group_locf'

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# Anxiety ####
## Soft ####

summon_km_curves(
  data = data_anxiety_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in UC/IBDU",
  xlab = "Time from 12 month landmark (months)",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme,
  break.time.by = 365/4,  
  xscale = 365/12,
  xlim = c(0, 365)
)

## Hard ####

summon_km_curves(
  data = data_anxiety_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in UC/IBDU",
  xlab = "Time from 12 month landmark (months)",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme,
  break.time.by = 365/4,  
  xscale = 365/12,
  xlim = c(0, 365)
)

# Depression ####
legend.title = 'HADS Depression Score'

## Soft ####

summon_km_curves(
  data = data_depression_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in UC/IBDU",
  xlab = "Time from 12 month landmark (months)",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme,
  break.time.by = 365/4,  
  xscale = 365/12,
  xlim = c(0, 365)
)

## Hard ####

summon_km_curves(
  data = data_depression_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in UC/IBDU",
  xlab = "Time from 12 month landmark (months)",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme,
  break.time.by = 365/4,  
  xscale = 365/12,
  xlim = c(0, 365)
)
