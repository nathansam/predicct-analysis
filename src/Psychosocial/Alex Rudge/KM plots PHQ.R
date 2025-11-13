library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# Plotting Kaplan-Meier curves
# Run PHQ

legend.title = 'Somatisation'
legend.labs = c('0-4 (none)', '5-9 (mild)', '10-14 (moderate)', '15-30 (severe)')
palette = NULL
dependent = 'somatisation'

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
  title = "Time to Objective Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# CD
summon_km_curves(
  data = data_survival_hard_cd,
  dependent = dependent,
  title = "Time to Objective Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

