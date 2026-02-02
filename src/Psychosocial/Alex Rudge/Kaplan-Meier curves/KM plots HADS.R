library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Plotting Kaplan-Meier curves
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# HADS Anxiety ####
legend.title = 'HADS anxiety score'
legend.labs = c('0-7', '8-21')
palette = okabe_ito
dependent = 'score_group'

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = 'bold')
  )

# Soft
# UC
plot_anxiety_soft_uc <- summon_km_curves(
  data = data_survival_anxiety_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_anxiety_soft_uc

# CD
plot_anxiety_soft_cd <- summon_km_curves(
  data = data_survival_anxiety_soft_cd,
  dependent = dependent,
  title = "Time to patient-reported Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_anxiety_soft_cd

# Hard
# UC
plot_anxiety_hard_uc <- summon_km_curves(
  data = data_survival_anxiety_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_anxiety_hard_uc

# CD
plot_anxiety_hard_cd <- summon_km_curves(
  data = data_survival_anxiety_hard_cd,
  dependent = dependent,
  title = "Time to objective flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_anxiety_hard_cd

# Save individual plots as rds

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Kaplan-Meier/"

# Soft uc
readr::write_rds(
  x = plot_anxiety_soft_uc,
  file = paste0(filepath_save, "plot_anxiety_soft_uc.rds")
)

# Soft cd
readr::write_rds(
  x = plot_anxiety_soft_cd,
  file = paste0(filepath_save, "plot_anxiety_soft_cd.rds")
)

# Hard uc
readr::write_rds(
  x = plot_anxiety_hard_uc,
  file = paste0(filepath_save, "plot_anxiety_hard_uc.rds")
)

# Hard cd
readr::write_rds(
  x = plot_anxiety_hard_cd,
  file = paste0(filepath_save, "plot_anxiety_hard_cd.rds")
)



# 4x4 plots

plot_anxiety <- summon_km_curves_panel(
  p1 = plot_anxiety_soft_cd,
  p2 = plot_anxiety_soft_uc,
  p3 = plot_anxiety_hard_cd,
  p4 = plot_anxiety_hard_uc
)

plot_anxiety

# Save as landscape 9 x 9 inches
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = paste0(filepath_save, "Kaplan Meier HADS Anxiety.pdf"),
  plot = plot_anxiety,
  width = 9,
  height = 9,
  units = 'in'
)



# HADS Depression ####
legend.title = 'HADS depression score'
legend.labs = c('0-7', '8-21')
palette = okabe_ito
dependent = 'score_group'


# Soft
# UC
plot_depression_soft_uc <- summon_km_curves(
  data = data_survival_depression_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_depression_soft_uc

# CD
plot_depression_soft_cd <- summon_km_curves(
  data = data_survival_depression_soft_cd,
  dependent = dependent,
  title = "Time to patient-reported flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_depression_soft_cd

# Hard
# UC
plot_depression_hard_uc <- summon_km_curves(
  data = data_survival_depression_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_depression_hard_uc

# CD
plot_depression_hard_cd <- summon_km_curves(
  data = data_survival_depression_hard_cd,
  dependent = dependent,
  title = "Time to objective flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_depression_hard_cd

# Save individual plots as rds

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Kaplan-Meier/"

# Soft uc
readr::write_rds(
  x = plot_depression_soft_uc,
  file = paste0(filepath_save, "plot_depression_soft_uc.rds")
)

# Soft cd
readr::write_rds(
  x = plot_depression_soft_cd,
  file = paste0(filepath_save, "plot_depression_soft_cd.rds")
)

# Hard uc
readr::write_rds(
  x = plot_depression_hard_uc,
  file = paste0(filepath_save, "plot_depression_hard_uc.rds")
)

# Hard cd
readr::write_rds(
  x = plot_depression_hard_cd,
  file = paste0(filepath_save, "plot_depression_hard_cd.rds")
)

# 4x4 plots

plot_depression <- summon_km_curves_panel(
  p1 = plot_depression_soft_cd,
  p2 = plot_depression_soft_uc,
  p3 = plot_depression_hard_cd,
  p4 = plot_depression_hard_uc
)

plot_depression

# Save as landscape 9 x 9 inches
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = paste0(filepath_save, "Kaplan Meier HADS Depression.pdf"),
  plot = plot_depression,
  width = 9,
  height = 9,
  units = 'in'
)
