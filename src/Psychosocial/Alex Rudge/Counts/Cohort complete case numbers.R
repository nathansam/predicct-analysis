library(tidyverse)
library(magrittr)


# Comparing the psychosocial cohort to the entire Predicct cohort


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-full.rds"
)

data_cd <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-cd.rds"
)

data_uc <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-uc.rds"
)

# Control scores
data_control <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/chiara/IBD_C.rds"
)


# Psychosocial cohort
# Load participants

participants <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds"
)

# Select relevant columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    diagnosis2,
    IMD,
    Sex,
    Age,
    FC,
    Smoke,
    SiteNo
  )


# Only psychosocial cohort
data_cohort %<>%
  dplyr::filter(
      ParticipantNo %in% participants$ParticipantNo
)


# How much missingness when doing complete case
data_cohort %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::summarise(
    n = n(),
    cc = sum(complete.cases(dplyr::pick(dplyr::everything()))),
    missing = n - cc,
    missing_percent = 100*missing/n
  )

# CD 11.9%
# UC 12.7%