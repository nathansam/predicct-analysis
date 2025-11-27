library(tidyverse)
library(magrittr)


# Load in all the Cox results

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Partially adjusted/"

# Suffix - cc (complete case) or mice 
#suffix <- "_cc.rds"

#suffix <- "_mice.rds"


# HADS
cox_results_hads_anxiety <- readr::read_rds(paste0(filepath, "cox_results_hads_anxiety", suffix))

cox_results_hads_depression <- readr::read_rds(paste0(filepath, "cox_results_hads_depression", suffix))

# Exercise
cox_results_exercise <- readr::read_rds(paste0(filepath, "cox_results_exercise", suffix))

# Life Events
cox_results_lifeevents <- readr::read_rds(paste0(filepath, "cox_results_lifeevents", suffix))

# PHQ
cox_results_phq <- readr::read_rds(paste0(filepath, "cox_results_phq", suffix))

# PSQI
cox_results_psqi <- readr::read_rds(paste0(filepath, "cox_results_psqi", suffix))

# Differentiate between anxiety and depression
cox_results_hads_anxiety %<>%
  dplyr::mutate(variable = "score_group_anxiety")

cox_results_hads_depression %<>%
  dplyr::mutate(variable = "score_group_depression")


# Combine
cox_results <- cox_results_hads_anxiety %>%
  dplyr::bind_rows(cox_results_hads_depression) %>%
  dplyr::bind_rows(cox_results_exercise) %>%
  dplyr::bind_rows(cox_results_lifeevents) %>%
  dplyr::bind_rows(cox_results_phq) %>%
  dplyr::bind_rows(cox_results_psqi)


# Ordering for plotting?
# Need to do manually
cox_results %<>%
  dplyr::mutate(
    ordering = dplyr::case_when(
      term == 'score_group0-7' ~ 0,
      term == 'score_group8-21' ~ 1,
      term == 'MinimumExerciseYes' ~ 0,
      term == 'MinimumExerciseNo' ~ 1,
      term == 'AnyLifeEventsNo' ~ 0,
      term == 'AnyLifeEventsYes' ~ 1,
      term == 'somatisationNone' ~ 0,
      term == 'somatisationMild' ~ 1,
      term == 'somatisationModSev' ~ 2,
      term == 'SleepDisturbanceNo' ~ 0,
      term == 'SleepDisturbanceYes' ~ 1,
    )
  )

# Tidy up the term
cox_results %<>%
  dplyr::mutate(
    term_tidy = dplyr::case_when(
      term == 'score_group0-7' & variable == 'score_group_anxiety' ~ 'HADS Anxiety Score 0-7',
      term == 'score_group8-21' & variable == 'score_group_anxiety' ~ 'HADS Anxiety Score 8-21',
      term == 'score_group0-7' & variable == 'score_group_depression' ~ 'HADS Depression Score 0-7',
      term == 'score_group8-21' & variable == 'score_group_depression' ~ 'HADS Depression Score 8-21',
      term == 'MinimumExerciseYes' ~ 'Meets recommended exercise',
      term == 'MinimumExerciseNo' ~ 'Does not meet recommended exercise',
      term == 'AnyLifeEventsNo' ~ 'No life events in past month',
      term == 'AnyLifeEventsYes' ~ 'At least 1 life event in past month',
      term == 'somatisationNone' ~ 'Somatisation 0-4 (none)',
      term == 'somatisationMild' ~ 'Somatisation 5-9 (mild)',
      term == 'somatisationModSev' ~ 'Somatisation 10-30 (moderate/severe)',
      term == 'SleepDisturbanceNo' ~ 'No sleep disturbance (PSQI<=5)',
      term == 'SleepDisturbanceYes' ~ 'Sleep disturbance (PSQI>5)',
    )
  )

# Confidence intervals
cox_results %<>%
  dplyr::mutate(
    conf.interval.tidy = dplyr::case_when(
      (is.na(conf.low) & is.na(conf.high)) ~ "-",
      TRUE ~ paste0(sprintf("%.3g", estimate), " (", sprintf("%.3g", conf.low), " - ", sprintf("%.3g", conf.high), ")")
    )
  )

# Tidy p values
cox_results %<>%
  dplyr::mutate(
    p.value.tidy = dplyr::case_when(
      is.na(p.value) ~ "-",
      TRUE ~ sprintf("%.3g", p.value)
    )
  )

# Significance
cox_results %<>%
  dplyr::mutate(
    significance = dplyr::case_when(
      is.na(p.value) ~ "Reference level",
      p.value <= 0.05 ~ "Significant",
      p.value > 0.05 ~ "Not Significant"
    )
  )


# Save
readr::write_rds(
  x = cox_results,
  file = paste0(filepath, "cox_results_all_variables", suffix)
)
