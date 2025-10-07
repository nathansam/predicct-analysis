library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# Plotting Kaplan-Meier curves
# Run Alcohol

legend.title = 'Weekly Units'
legend.labs = c('0-0.1', '0.1-14', 'Over 14')
palette = NULL
dependent = 'weekly_units'

# Soft
# UC
summon_km_curves(
  data = data_survival_soft_uc,
  dependent = dependent,
  title = "Time to Patient Reported Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# CD
summon_km_curves(
  data = data_survival_soft_cd,
  dependent = dependent,
  title = "Time to Patient Reported Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Hard
# UC
summon_km_curves(
  data = data_survival_hard_uc,
  dependent = dependent,
  title = "Time to Hard Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# CD
summon_km_curves(
  data = data_survival_hard_cd,
  dependent = dependent,
  title = "Time to Hard Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

