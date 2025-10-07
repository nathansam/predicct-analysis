library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# Plotting Kaplan-Meier curves
# Run Exercise.qmd

legend.title = 'Acheived minimum exercise'
legend.labs = c("Yes", "No")
palette = NULL
dependent = 'MinimumExercise'

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

