library(tidyverse)
library(magrittr)


# Load in all the Cox results

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# HADS
cox_results_hads_anxiety <- readr::read_rds(paste0(filepath, "cox_results_hads_anxiety.rds"))

cox_results_hads_depression <- readr::read_rds(paste0(filepath, "cox_results_hads_depression.rds"))

# Exercise
cox_results_exercise <- readr::read_rds(paste0(filepath, "cox_results_exercise.rds"))

# Alcohol
cox_results_alcohol <- readr::read_rds(paste0(filepath, "cox_results_alcohol.rds"))

# Life Events
cox_results_lifeevents <- readr::read_rds(paste0(filepath, "cox_results_lifeevents.rds"))

# PHQ
cox_results_phq <- readr::read_rds(paste0(filepath, "cox_results_phq.rds"))

# PSQI
cox_results_psqi <- readr::read_rds(paste0(filepath, "cox_results_psqi.rds"))

# Differentiate between anxiety and depression
cox_results_hads_anxiety %<>%
  dplyr::mutate(variable = "score_group_anxiety")

cox_results_hads_depression %<>%
  dplyr::mutate(variable = "score_group_depression")


# Combine
cox_results <- cox_results_hads_anxiety %>%
  dplyr::bind_rows(cox_results_hads_depression) %>%
  dplyr::bind_rows(cox_results_exercise) %>%
  dplyr::bind_rows(cox_results_alcohol) %>%
  dplyr::bind_rows(cox_results_lifeevents) %>%
  dplyr::bind_rows(cox_results_phq) %>%
  dplyr::bind_rows(cox_results_psqi)


# Ordering for plotting?
# Need to do manually really
cox_results %<>%
  dplyr::mutate(
    ordering = dplyr::case_when(
      term == 'score_group0-7' ~ 0,
      term == 'score_group8-10' ~ 1,
      term == 'score_group11-21' ~ 2,
      term == 'MinimumExerciseYes' ~ 0,
      term == 'MinimumExerciseNo' ~ 1,
      term == 'weekly_units0-0.1' ~ 0,
      term == 'weekly_units0.1-14' ~ 1,
      term == 'weekly_units>14' ~ 2,
      term == 'AnyLifeEventsNo' ~ 0,
      term == 'AnyLifeEventsYes' ~ 1,
      term == 'somatisationNone' ~ 0,
      term == 'somatisationMild' ~ 1,
      term == 'somatisationModerate' ~ 2,
      term == 'somatisationSevere' ~ 3,
      term == 'SleepDisturbanceNo' ~ 0,
      term == 'SleepDisturbanceYes' ~ 1,
    )
  )




# Save
readr::write_rds(
  x = cox_results,
  file = paste0(filepath, "cox_results_all_variables.rds")
)
