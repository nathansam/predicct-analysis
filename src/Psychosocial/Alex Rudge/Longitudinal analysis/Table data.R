library(tidyverse)
library(magrittr)
library(gtsummary)


# Demographics of psychosocial participants who did and did not respond
# to the 12 and 24 month questionnaires


# Load in the Predicct cohort
data_cohort <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-full.rds"
)

# Flare data
flares_soft <- readRDS(paste0("/Volumes/igmm/cvallejo-predicct/people/chiara/", "flares_soft.RDS"))
flares_hard <- readRDS(paste0("/Volumes/igmm/cvallejo-predicct/people/chiara/", "flares_hard.RDS"))

# Participants in the psychosocial cohort
participants <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds"
)

# Questionnaires
filepath <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

# HADS
hads <- readr::read_rds(glue::glue("{filepath}hads_long.rds"))

# PHQ
phq <- readr::read_rds(glue::glue("{filepath}phq_long.rds"))

# Exercise
exercise <- readr::read_rds(glue::glue("{filepath}exercise_long.rds"))

# Sleep disturbance
psqi <- readr::read_rds(glue::glue("{filepath}psqi_long.rds"))


# Create df
data <- tidyr::expand_grid(ParticipantNo = participants$ParticipantNo,
                           month = c(0, 12, 24))

# Add diagnosis
data %<>% 
  dplyr::left_join(participants)


# Add demographics
data_cohort %<>%
  dplyr::select(
    ParticipantNo, SiteNo, Age, Sex, Ethnicity, IMD, Smoke, FC 
  )

data %<>% dplyr::left_join(data_cohort)

# Add questionnaire results
# HADS
hads %<>%
  dplyr::select(ParticipantNo, month, anxiety, depression)

# Calculate anxiety and depression score
hads %<>%
  dplyr::mutate(
    anxiety_group = dplyr::case_when(
      anxiety <= 7 ~ "0-7",
      anxiety > 7 ~ "8-21"
    ),
    depression_group = dplyr::case_when(
      depression <= 7 ~ "0-7",
      depression > 7 ~ "8-21"
    )
  )

# Join
data %<>% dplyr::left_join(hads, by = c("ParticipantNo", "month"))

# PHQ
phq %<>%
  dplyr::select(ParticipantNo, month, somatisation)

data %<>% dplyr::left_join(phq, by = c("ParticipantNo", "month"))

# Exercise
exercise %<>%
  dplyr::select(ParticipantNo, month, MinimumExercise)

data %<>% dplyr::left_join(exercise, by = c("ParticipantNo", "month"))


# Sleep
psqi %<>%
  dplyr::select(ParticipantNo, month, SleepDisturbance)

data %<>% dplyr::left_join(psqi, by = c("ParticipantNo", "month"))

# Flare data
flares_soft %<>% dplyr::select(ParticipantNo, softflare, softflare_time)
flares_hard %<>% dplyr::select(ParticipantNo, hardflare, hardflare_time)

data %<>% 
  dplyr::left_join(flares_soft) %>%
  dplyr::left_join(flares_hard)

# Save

readr::write_rds(
  x = data,
  file = glue::glue("{filepath}followup.rds")
)
