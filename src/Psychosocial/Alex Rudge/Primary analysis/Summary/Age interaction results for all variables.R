library(tidyverse)
library(magrittr)


# Load in all age interaction results

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Primary analysis/Interactions/"

# HADS
cox_results_hads_anxiety <- readr::read_rds(
  paste0(filepath, "cox_results_hads_anxiety_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "hads_anxiety")

cox_results_hads_depression <- readr::read_rds(
  paste0(filepath, "cox_results_hads_depression_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "hads_depression")

# PHQ
cox_results_somatisation <- readr::read_rds(
  paste0(filepath, "cox_results_phq_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "somatisation")

# Fatigue
cox_results_fatigue <- readr::read_rds(
  paste0(filepath, "cox_results_fatigue_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "OftenLackEnergy")

# PSQI
cox_results_psqi <- readr::read_rds(
  paste0(filepath, "cox_results_psqi_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "SleepDisturbance")

# Exercise
cox_results_exercise <- readr::read_rds(
  paste0(filepath, "cox_results_exercise_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "MinimumExercise")

# Life Events
cox_results_lifeevents <- readr::read_rds(
  paste0(filepath, "cox_results_lifeevents_interaction_age_mice.rds")
) %>%
  dplyr::mutate(variable = "AnyLifeEvents")


# Combine
cox_results_age_interactions <- cox_results_hads_anxiety %>%
  dplyr::bind_rows(cox_results_hads_depression) %>%
  dplyr::bind_rows(cox_results_somatisation) %>%
  dplyr::bind_rows(cox_results_fatigue) %>%
  dplyr::bind_rows(cox_results_psqi) %>%
  dplyr::bind_rows(cox_results_exercise) %>%
  dplyr::bind_rows(cox_results_lifeevents)


# Split by flare type and diagnosis
cox_results_soft_uc <- cox_results_age_interactions %>%
  dplyr::filter(
    flare_type == "soft",
    diagnosis2 == "UC/IBDU"
  )

cox_results_soft_cd <- cox_results_age_interactions %>%
  dplyr::filter(
    flare_type == "soft",
    diagnosis2 == "CD"
  )

cox_results_hard_uc <- cox_results_age_interactions %>%
  dplyr::filter(
    flare_type == "hard",
    diagnosis2 == "UC/IBDU"
  )

cox_results_hard_cd <- cox_results_age_interactions %>%
  dplyr::filter(
    flare_type == "hard",
    diagnosis2 == "CD"
  )
