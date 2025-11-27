library(tidyverse)
library(magrittr)

# Comparing the psychosocial cohort to the entire Predicct cohort


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/flares-controlled.RDS"
)

# Psychosocial cohort
# I will just use the HADS cohort for now

# So run HADS data cleaning

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    diagnosis2,
    softflare,
    softflare_time,
    hardflare,
    hardflare_time
  )

# Flag if a patient is in the psychosocial cohort
data_cohort %<>%
  dplyr::mutate(
    psychosocial = dplyr::case_when(
      ParticipantNo %in% hads$ParticipantNo ~ 'Yes',
      .default = 'No'
    )
  )

# How many in psychosocial cohort
data_cohort %>%
  dplyr::count(psychosocial)


# Is missing being in the psychosocial cohort informative of flare?

data_cohort_soft_uc <- data_cohort %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

data_cohort_soft_cd <- data_cohort %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

# Hard flare
data_cohort_hard_uc <- data_cohort %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)

data_cohort_hard_cd <- data_cohort %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)



legend.title = 'Responded to questionnaire'
legend.labs = c('No', 'Yes')
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette = okabe_ito
dependent = 'psychosocial'


# Soft
# UC
plot_soft_uc <- summon_km_curves(
  data = data_cohort_soft_uc,
  dependent = dependent,
  title = "Time to patient reported flare in ulcerative colitis",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_soft_uc

# CD
plot_soft_cd <- summon_km_curves(
  data = data_cohort_soft_cd,
  dependent = dependent,
  title = "Time to patient reported flare in Crohn's disease",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_soft_cd

# Very - needs mentioning in the paper.


# UC
plot_hard_uc <- summon_km_curves(
  data = data_cohort_hard_uc,
  dependent = dependent,
  title = "Time to objective flare in ulcerative colitis",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_hard_uc

# CD
plot_hard_cd <- summon_km_curves(
  data = data_cohort_hard_cd,
  dependent = dependent,
  title = "Time to objective flare in Crohn's disease",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

plot_hard_cd

# But not hard flare.

# As a 2x2 plot
plot <- summon_km_curves_panel(
  plot_soft_uc,
  plot_soft_cd,
  plot_hard_uc,
  plot_hard_cd
)

plot

# Save

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = paste0(filepath_save, "Kaplan Meier Questionnaire Response.pdf"),
  plot = plot,
  width = 9.5,
  height = 8.5,
  units = 'in'
)

