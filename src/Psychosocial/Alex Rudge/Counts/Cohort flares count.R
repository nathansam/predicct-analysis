library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Flare data
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"

data_survival_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
data_survival_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))

# Participants
participants <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds"
)


# Rename columns
data_survival_soft %<>%
  dplyr::rename(
    DiseaseFlareYN = softflare, time = softflare_time
  )

data_survival_hard %<>%
  dplyr::rename(
    DiseaseFlareYN = hardflare, time = hardflare_time
  )

# Only select psychosocial cohort with inner join
data_survival_soft %<>%
  dplyr::inner_join(participants, by = 'ParticipantNo')

data_survival_hard %<>%
  dplyr::inner_join(participants, by = 'ParticipantNo')

# How many patients do we have soft/hard flare data for
data_survival_soft %>%
  dplyr::count(diagnosis2)
# 1826 (CD 922, UC/IBDU 904) with soft flare data
# Therefore missing 935 - 922 = 13 CD, 920 - 904 = 16 UC

data_survival_hard %>%
  dplyr::count(diagnosis2)
# 1770 (CD 891, UC/IBDU 897) with hard flare data
# Therefore missing 935 - 891 = 44 CD, 920 - 897 = 23 UC

# How many having flares in the 24 months?
data_survival_soft %>%
  dplyr::count(diagnosis2, DiseaseFlareYN) %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::mutate(cum = n/sum(n))
# Total 638 (CD 302, UC/IBDU 336) with soft flare data
# Cumulative rate: Overall 638/1826 = 35.0%. CD 32.8%, UC 37.2%

data_survival_hard %>%
  dplyr::count(diagnosis2, DiseaseFlareYN) %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::mutate(cum = n/sum(n))
# Total 230 (CD 105, UC/IBDU 125) with hard flare data
# Cumulative rate: Overall 230/1770 = 13.0%. CD 11.8%, UC 14.2%

# How many missing?



# Kaplan-Meier
legend.title = 'IBD Type'
legend.labs = c('CD', 'UC/IBDU')
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette = okabe_ito
dependent = 'diagnosis2'

plot_soft <- summon_km_curves(
  data = data_survival_soft,
  dependent = dependent,
  title = "Time to patient-reported flare",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  fun = "event"
)

plot_hard <- summon_km_curves(
  data = data_survival_hard,
  dependent = dependent,
  title = "Time to objective flare",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette,
  fun = "event"
)


# Side by side
layout <- "
AAAA
BBDD
CCEE
"

# Max y so both plots have the same scale
ymax = 0.42

plot <- patchwork::guide_area() +
  (plot_soft$plot + ylim(0, ymax)) + plot_soft$table +
  (plot_hard$plot + ylim(0, ymax)) + plot_hard$table +
  patchwork::plot_layout(
    design = layout,
    heights = c(0.15, 3, 0.8),
    guides = 'collect'
  )

plot

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

# ggsave(
#   filename = paste0(filepath_save, "Cumulative events IBD type.pdf"),
#   plot = plot,
#   width = 9.5,
#   height = 5,
#   units = 'in'
# )
