library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# Plotting Kaplan-Meier curves

# HADS Anxiety ####
legend.title = 'HADS Anxiety Score'
legend.labs = c('0-7', '8-10', '11-21')
palette = NULL
dependent = 'score_group'

# Soft
# UC
plot_anxiety_soft_uc <- summon_km_curves(
  data = data_survival_anxiety_soft_uc,
  dependent = dependent,
  title = "Time to Patient Reported Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_anxiety_soft_uc

# CD
plot_anxiety_soft_cd <- summon_km_curves(
  data = data_survival_anxiety_soft_cd,
  dependent = dependent,
  title = "Time to Patient Reported Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_anxiety_soft_cd

# Hard
# UC
plot_anxiety_hard_uc <- summon_km_curves(
  data = data_survival_anxiety_hard_uc,
  dependent = dependent,
  title = "Time to Objective Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_anxiety_hard_uc

# CD
plot_anxiety_hard_cd <- summon_km_curves(
  data = data_survival_anxiety_hard_cd,
  dependent = dependent,
  title = "Time to Objective Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_anxiety_hard_cd

# 4x4 plots

summon_km_curves_panel(
  p1 = plot_anxiety_soft_uc,
  p2 = plot_anxiety_soft_cd,
  p3 = plot_anxiety_hard_uc,
  p4 = plot_anxiety_hard_cd
)

# Save as landscape 10 x 11 inches




# HADS Depression ####
legend.title = 'HADS Depression Score'
legend.labs = c('0-7', '8-10', '11-21')
palette = NULL
dependent = 'score_group'


# Soft
# UC
summon_km_curves(
  data = data_survival_depression_soft_uc,
  dependent = dependent,
  title = "Time to Patient Reported Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# CD
summon_km_curves(
  data = data_survival_depression_soft_cd,
  dependent = dependent,
  title = "Time to Patient Reported Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Hard
# UC
summon_km_curves(
  data = data_survival_depression_hard_uc,
  dependent = dependent,
  title = "Time to Objective Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# CD
summon_km_curves(
  data = data_survival_depression_hard_cd,
  dependent = dependent,
  title = "Time to Objective Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)
