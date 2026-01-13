library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Plotting Kaplan-Meier curves
# Run PHQ
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

legend.title = 'Somatisation'
legend.labs = c('None', 'Mild', 'Moderate/Severe')
palette = okabe_ito
dependent = 'somatisation'

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# Soft
# UC
plot_soft_uc <- summon_km_curves(
  data = data_survival_soft_uc,
  dependent = dependent,
  title = "Time to patient-reported flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_soft_uc

# CD
plot_soft_cd <- summon_km_curves(
  data = data_survival_soft_cd,
  dependent = dependent,
  title = "Time to patient reported flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_soft_cd

# Hard
# UC
plot_hard_uc <- summon_km_curves(
  data = data_survival_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in UC/IBDU",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_hard_uc

# CD
plot_hard_cd <- summon_km_curves(
  data = data_survival_hard_cd,
  dependent = dependent,
  title = "Time to objective flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  ggtheme = custom_theme
)

plot_hard_cd

# Save individual plots as rds

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Kaplan-Meier/"

# Soft uc
readr::write_rds(
  x = plot_soft_uc,
  file = paste0(filepath_save, "plot_phq_soft_uc.rds")
)

# Soft cd
readr::write_rds(
  x = plot_soft_cd,
  file = paste0(filepath_save, "plot_phq_soft_cd.rds")
)

# Hard uc
readr::write_rds(
  x = plot_hard_uc,
  file = paste0(filepath_save, "plot_phq_hard_uc.rds")
)

# Hard cd
readr::write_rds(
  x = plot_hard_cd,
  file = paste0(filepath_save, "plot_phq_hard_cd.rds")
)


# 4x4 plots

plot <- summon_km_curves_panel(
  p1 = plot_soft_cd,
  p2 = plot_soft_uc,
  p3 = plot_hard_cd,
  p4 = plot_hard_uc
) & theme(axis.title.y = element_text(vjust = -20))

plot

# Save as landscape 10 x 11 inches
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = paste0(filepath_save, "Kaplan Meier PHQ.pdf"),
  plot = plot,
  width = 10,
  height = 9,
  units = 'in'
)

