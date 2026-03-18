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


# Total follow-up
# Soft
data_survival_soft %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::summarise(
    total = sum(time),
    total_years = total/365.25,
    mean = mean(time),
    sd = sd(time)
  )

# Hard
data_survival_hard %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::summarise(
    total = sum(time),
    total_years = total/365.25,
    mean = mean(time),
    sd = sd(time)
  )


# How many censored?
data_survival_soft %>%
  dplyr::filter(DiseaseFlareYN == 0 & time < 730) %>%
  dplyr::count(diagnosis2)

data_survival_hard %>%
  dplyr::filter(DiseaseFlareYN == 0 & time < 730) %>%
  dplyr::count(diagnosis2)
